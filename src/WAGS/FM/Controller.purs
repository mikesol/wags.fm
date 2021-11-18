module WAGS.FM.Controller where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Parallel (parTraverse)
import Control.Promise (toAffE)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (fold, for_)
import Data.Int (round)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (error, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (new, read, write)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (create, subscribe)
import FRP.Event as Event
import Foreign (Foreign)
import Foreign.Index (readProp)
import JIT.API as API
import JIT.Compile (compile)
import JIT.EvalSources (evalSources)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)
import WAGS.FM.Emitter (loopEmitter)
import WAGS.Interpret (close, defaultFFIAudio, makeUnitCache, constant0Hack, context, contextResume, contextState)
import WAGS.Lib.Learn (FullSceneBuilder(..), Analysers, easingAlgorithm)
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (SampleCache, TidalRes)
import WAGS.Lib.Tidal.Util (doDownloads')
import WAGS.Run (run, Run)

----------
loaderUrl :: String
loaderUrl = "https://purescript-wags.netlify.app/js/output"

compileUrl :: String
compileUrl = "https://supvghemaw.eu-west-1.awsapprunner.com"

----------

type Playlist = Array { code :: String, wag :: AFuture, duration :: Number }
type SetPlaylist = Playlist -> Effect Unit
----------
type ScrollIndex = Int
type SetScrollIndex = Int -> Effect Unit
----------
type SetCode = String -> Effect Unit
----------
type IsScrolling = Boolean
type SetIsScrolling = Boolean -> Effect Unit
----------
type IsPlaying = Boolean
type SetIsPlaying = Boolean -> Effect Unit
----------
type StopScrolling = Effect Unit
type SetStopScrolling = Effect Unit -> Effect Unit
----------
type StopWags = Effect Unit
type SetStopWags = Effect Unit -> Effect Unit
----------
type NewWagPush = AFuture -> Effect Unit
type SetNewWagPush = (AFuture -> Effect Unit) -> Effect Unit

type PauseScrollSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setIsScrolling :: SetIsScrolling
  }
  -> Effect Unit

----

type StopWagsSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setIsScrolling :: SetIsScrolling
  , setIsPlaying :: SetIsPlaying
  , stopWags :: StopWags
  , setStopWags :: SetStopWags
  }
  -> Effect Unit

---

type PlayScrollSig =
  { scrollIndex :: ScrollIndex
  , setCode :: SetCode
  , cleanErrorState :: Effect Unit
  , setStopScrolling :: SetStopScrolling
  , setScrollIndex :: SetScrollIndex
  , isScrolling :: IsScrolling
  , setIsScrolling :: SetIsScrolling
  , newWagPush :: NewWagPush
  , currentPlaylist :: Playlist
  , setCurrentPlaylist :: SetPlaylist
  , compileOnPlay :: Boolean
  , code :: String
  , ourFaultErrorCallback :: Error -> Effect Unit
  , yourFaultErrorCallback :: Array API.CompilerError -> Effect Unit
  }
  -> Effect Unit

---

type PlayWagsSig =
  { scrollIndex :: ScrollIndex
  , setCode :: SetCode
  , stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setScrollIndex :: SetScrollIndex
  , setNewWagPush :: SetNewWagPush
  , setIsScrolling :: SetIsScrolling
  , isPlaying :: IsPlaying
  , setIsPlaying :: SetIsPlaying
  , setStopWags :: SetStopWags
  , bufferCache :: { read :: Effect SampleCache, write :: SampleCache -> Effect Unit }
  , currentPlaylist :: Playlist
  }
  -> Effect Unit

readableToBehavior :: Effect ~> Behavior
readableToBehavior r = behavior \e ->
  Event.makeEvent \f ->
    Event.subscribe e \v ->
      r >>= f <<< v

stopWags :: StopWagsSig
stopWags
  { setIsScrolling
  , setIsPlaying
  , stopScrolling
  , setStopScrolling
  , stopWags: sw
  , setStopWags
  } =
  setIsScrolling false
    *> setIsPlaying false
    *> stopScrolling
    *> setStopScrolling (pure unit)
    *> sw
    *> setStopWags (pure unit)

pauseScroll :: PauseScrollSig
pauseScroll { setIsScrolling, stopScrolling, setStopScrolling } =
  setIsScrolling false *> stopScrolling *> setStopScrolling (pure unit)

al :: NonEmpty Array ~> NonEmpty List
al (a :| b) = a :| (List.fromFoldable b)

playScroll :: PlayScrollSig
playScroll
  { scrollIndex
  , cleanErrorState
  , setScrollIndex
  , isScrolling
  , setIsScrolling
  , setStopScrolling
  , newWagPush
  , compileOnPlay
  , ourFaultErrorCallback
  , yourFaultErrorCallback
  , code
  , setCode
  , currentPlaylist
  , setCurrentPlaylist
  } =
  for_ (NEA.fromArray currentPlaylist) \nea' ->
    when (not isScrolling) $ launchAff_ do
      liftEffect $ cleanErrorState
      nea <- map (al <<< NEA.toNonEmpty) $
        if not compileOnPlay then pure nea'
        else makeAff \cb -> do
          compile
            { code
            , loaderUrl
            , compileUrl
            , ourFaultErrorCallback: \err -> do
                ourFaultErrorCallback err
                cb $ Left err
            , yourFaultErrorCallback: \err -> do
                yourFaultErrorCallback err
                cb $ Left $ error $ JSON.writeJSON err
            , successCallback: \{ js } -> do
                wag' <- liftEffect $ evalSources js
                  >>= runExceptT <<< readProp "wag"
                  >>= either (throwError <<< error <<< show) pure
                let wag = (unsafeCoerce :: Foreign -> AFuture) wag'
                let
                  newNea = fromMaybe nea' $ NEA.modifyAt (scrollIndex `mod` NEA.length nea')
                    (_ { wag = wag, code = code })
                    nea'
                setCurrentPlaylist $ NEA.toArray newNea
            }
          mempty
      liftEffect do
        pg <- new scrollIndex
        stopScrolling <- subscribe
          (loopEmitter (_.duration >>> mul 1000.0 >>> round) $ nea)
          \{ wag, code: upcomingCode } -> do
            pg' <- read pg
            let np = pg' + 1
            setScrollIndex np
            setCode upcomingCode
            write np pg
            newWagPush wag
        setIsScrolling true
        setStopScrolling stopScrolling

playWags :: PlayWagsSig
playWags
  { scrollIndex
  , stopScrolling
  , setStopScrolling
  , setScrollIndex
  , setIsScrolling
  , setNewWagPush
  , isPlaying
  , setIsPlaying
  , bufferCache
  , setCode
  , setStopWags
  , currentPlaylist
  } =
  when (not isPlaying) do
    -- set is playing immediately
    -- note that we may want to pass a cancellation for the aff to avoid a race condition
    -- where the turn-off functionality is not set yet
    setIsPlaying true
    -- we should never have to stop scrolling, but we do just
    -- to make sure there was not an application error before
    audioCtx <- context
    waStatus <- liftEffect $ contextState audioCtx
    -- void the constant 0 hack
    -- this will result in a very slight performance decrease but makes iOS and Mac more sure
    _ <- liftEffect $ constant0Hack audioCtx
    unitCache <- liftEffect makeUnitCache
    stopScrolling
    { event, push } <- create
    launchAff_ do
      when (waStatus /= "running") (toAffE $ contextResume audioCtx)
      map fold $ parTraverse
        (doDownloads' audioCtx bufferCache (pure $ pure unit) identity)
        (map _.wag currentPlaylist)
      let FullSceneBuilder { triggerWorld, piece } = engine (pure unit) (map (const <<< const) event) $ (Left (readableToBehavior bufferCache.read))
      trigger /\ world <- snd $ triggerWorld (audioCtx /\ (pure (pure {} /\ pure {})))
      unsub <- liftEffect $ subscribe
        (run trigger world { easingAlgorithm } (defaultFFIAudio audioCtx unitCache) piece)
        (\(_ :: Run TidalRes Analysers) -> pure unit)
      liftEffect do
        setNewWagPush push
        setStopWags do
          unsub
          close audioCtx
        playScroll
          { scrollIndex
          -- no compile as we are restarting
          , compileOnPlay: false
          -- all of the memptys are a no-op as there is no compile step
          -- code is a no-op as there is no compile step
          -- to-do, group all the no-ops so we don't have all of these monoids
          -- floating around. they should be under one "maybe"
          , code: mempty
          , cleanErrorState: mempty
          , setCurrentPlaylist: mempty
          , ourFaultErrorCallback: mempty
          , yourFaultErrorCallback: mempty
          , setScrollIndex
          , setCode
          , isScrolling: false
          , setIsScrolling
          , setStopScrolling
          , newWagPush: push
          , currentPlaylist
          }

initialSampleCache :: SampleCache
initialSampleCache = Map.empty