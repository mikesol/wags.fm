module WAGS.FM.Controller where

import Prelude

import Control.Parallel (parTraverse)
import Control.Promise (toAffE)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Int (round)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (create, subscribe)
import FRP.Event as Event
import WAGS.FM.Emitter (loopEmitter)
import WAGS.FM.Types (Playlist)
import WAGS.Interpret (close, defaultFFIAudio, makeUnitCache, constant0Hack, context, contextResume, contextState)
import WAGS.Lib.Learn (FullSceneBuilder(..), easingAlgorithm)
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (SampleCache)
import WAGS.Lib.Tidal.Util (doDownloads')
import WAGS.Run (run, Run)

type Snippet = Int
type SetSnippet = Int -> Effect Unit
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
  { snippet :: Snippet
  , setStopScrolling :: SetStopScrolling
  , setSnippet :: SetSnippet
  , isScrolling :: IsScrolling
  , setIsScrolling :: SetIsScrolling
  , newWagPush :: NewWagPush
  , playlist :: Playlist
  }
  -> Effect Unit

---

type PlayWagsSig =
  { snippet :: Snippet
  , stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setSnippet :: SetSnippet
  , setIsScrolling :: SetIsScrolling
  , isPlaying :: IsPlaying
  , setIsPlaying :: SetIsPlaying
  , setStopWags :: SetStopWags
  , bufferCache :: { read :: Effect SampleCache, write :: SampleCache -> Effect Unit }
  , playlist :: Playlist
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
  { snippet
  , setSnippet
  , isScrolling
  , setIsScrolling
  , setStopScrolling
  , newWagPush
  , playlist
  } =
  for_ (map (al <<< NEA.toNonEmpty) (NEA.fromArray playlist)) \nea ->
    when (not isScrolling) do
      pg <- new snippet
      stopScrolling <- subscribe (loopEmitter (_.duration >>> mul 1000.0 >>> round) $ nea) \{ wag } -> do
        pg' <- read pg
        let np = pg' + 1
        setSnippet np
        write np pg
        newWagPush wag
      setIsScrolling true
      setStopScrolling stopScrolling

playWags :: PlayWagsSig
playWags
  { snippet
  , stopScrolling
  , setStopScrolling
  , setSnippet
  , setIsScrolling
  , isPlaying
  , setIsPlaying
  , bufferCache
  , setStopWags
  , playlist
  } =
  when (not isPlaying) do
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
    setIsPlaying true
    launchAff_ do
      when (waStatus /= "running") (toAffE $ contextResume audioCtx)
      fold <$> parTraverse (doDownloads' audioCtx bufferCache (pure $ pure unit) identity) (map _.wag playlist)
      let FullSceneBuilder { triggerWorld, piece } = engine (pure unit) (map (const <<< const) event) $ (Left (readableToBehavior bufferCache.read))
      trigger /\ world <- snd $ triggerWorld (audioCtx /\ (pure (pure {} /\ pure {})))
      unsub <- liftEffect $ subscribe
        (run trigger world { easingAlgorithm } (defaultFFIAudio audioCtx unitCache) piece)
        (\(_ :: Run Unit ()) -> pure unit)
      liftEffect $ setStopWags do
        unsub
        close audioCtx
      liftEffect $ playScroll
        { snippet
        , setSnippet
        , isScrolling: false
        , setIsScrolling
        , setStopScrolling
        , newWagPush: push
        , playlist
        }

initialSampleCache :: SampleCache
initialSampleCache = Map.empty