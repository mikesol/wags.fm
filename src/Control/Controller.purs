module Control.Controller where

import Prelude

import Control.Emitter (loopEmitter)
import Control.Monad.Except (runExceptT, throwError)
import Control.Parallel (parTraverse)
import Control.Promise (toAffE)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Foldable (fold, for_)
import Data.Int (round)
import Data.List (List)
import Data.List as List
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (error, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (new, read, write)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Event (create, subscribe)
import FRP.Event as Event
import Foreign (Foreign)
import Foreign.Index (readProp)
import Foreign.Object as Object
import JIT.API as API
import JIT.Compile (compile)
import JIT.EvalSources (evalSources)
import JIT.Loader (Loader)
import Types as T
import Unsafe.Coerce (unsafeCoerce)
import Util (sanitizePS)
import WAGS.Interpret (close, constant0Hack, context, contextResume, contextState, makeFFIAudioSnapshot)
import WAGS.Lib.Learn (FullSceneBuilder(..), Analysers, easingAlgorithm)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Tidal (openFuture)
import WAGS.Lib.Tidal.Types (AFuture, SampleCache, TidalRes)
import WAGS.Lib.Tidal.Util (doDownloads', r2b)
import WAGS.Run (run, BehavingRun)
import WAGS.WebAPI (AudioContext)

-----------
-- Types --

type BufferCache = { read :: Effect SampleCache, write :: SampleCache -> Effect Unit }

----------
loaderUrl :: String
loaderUrl = "https://purescript-wags.netlify.app/js/output"

compileUrl :: String
compileUrl = "https://ntjkvnw2c5.execute-api.eu-west-1.amazonaws.com"

----------
type SetAudioContext = AudioContext -> Effect Unit
----------
type SetPlaylist = T.PlaylistSequence -> Effect Unit
----------
type SetTidalRes = TidalRes -> Effect Unit
----------
type Cursor = Int
type SetCursor = Int -> Effect Unit
----------
type ScrollState = T.ScrollState
type SetScrollState = T.ScrollState -> Effect Unit
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

---
type MoveCursor =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setScrollState :: SetScrollState
  , cursor :: Cursor
  , setCursor :: SetCursor
  , newWagPush :: NewWagPush
  , nea :: T.PlaylistSequence
  }
  -> Effect Unit

---
type PauseScrollSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setScrollState :: SetScrollState
  }
  -> Effect Unit

----

type StopWagsSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setScrollState :: SetScrollState
  , setIsPlaying :: SetIsPlaying
  , stopWags :: StopWags
  , setCursor :: SetCursor
  , setStopWags :: SetStopWags
  }
  -> Effect Unit

---

type PlayScrollSig =
  { cursor :: Cursor
  , setStopScrolling :: SetStopScrolling
  , setCursor :: SetCursor
  , scrollState :: ScrollState
  , setScrollState :: SetScrollState
  , newWagPush :: NewWagPush
  , currentPlaylist :: T.PlaylistSequence
  , compileOnPlay ::
      Maybe
        { code :: String
        , loader :: Loader
        , cleanErrorState :: Effect Unit
        , setCurrentPlaylist :: SetPlaylist
        , ourFaultErrorCallback :: Error -> Effect Unit
        , yourFaultErrorCallback :: Array API.CompilerError -> Effect Unit
        }
  , bufferCache :: BufferCache
  , audioContext :: AudioContext
  }
  -> Effect Unit

type LaunchScrollSig =
  { cursor :: Cursor
  , setStopScrolling :: SetStopScrolling
  , setCursor :: SetCursor
  , setScrollState :: SetScrollState
  , newWagPush :: NewWagPush
  , nea :: T.PlaylistSequence
  }
  -> Effect Unit

---

type PlayWagsSig =
  { cursor :: Cursor
  , stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setCursor :: SetCursor
  , setNewWagPush :: SetNewWagPush
  , setScrollState :: SetScrollState
  , setAudioContext :: SetAudioContext
  , isPlaying :: IsPlaying
  , bufferCache :: BufferCache
  , setIsPlaying :: SetIsPlaying
  , setStopWags :: SetStopWags
  , setTidalRes :: SetTidalRes
  , currentPlaylist :: T.PlaylistSequence
  }
  -> Effect Unit

readableToBehavior :: Effect ~> Behavior
readableToBehavior r = behavior \e ->
  Event.makeEvent \f ->
    Event.subscribe e \v ->
      r >>= f <<< v

stopWags :: StopWagsSig
stopWags
  { setScrollState
  , setIsPlaying
  , stopScrolling
  , setStopScrolling
  , setCursor
  , stopWags: sw
  , setStopWags
  } =
  setScrollState T.Paused
    *> setIsPlaying false
    *> setCursor (-1)
    *> stopScrolling
    *> setStopScrolling (pure unit)
    *> sw
    *> setStopWags (pure unit)

pauseScroll :: PauseScrollSig
pauseScroll { setScrollState, stopScrolling, setStopScrolling } =
  setScrollState T.Paused
    *> stopScrolling
    *> setStopScrolling (pure unit)

moveCursor :: MoveCursor
moveCursor
  { stopScrolling
  , setStopScrolling
  , setScrollState
  , cursor
  , setCursor
  , newWagPush
  , nea
  } =
  stopScrolling
    *> setStopScrolling (pure unit)
    *> setScrollState T.Scrolling
    *> launchScroll
      { setStopScrolling
      , setScrollState
      , cursor
      , setCursor
      , newWagPush
      , nea
      }

al :: NonEmpty Array ~> NonEmpty List
al (a :| b) = a :| (List.fromFoldable b)

nea2nel :: NonEmptyArray ~> NonEmptyList
nea2nel a = NonEmptyList (h :| List.fromFoldable t)
  where
  h :| t = NEA.toNonEmpty a

nel2nea :: NonEmptyList ~> NonEmptyArray
nel2nea a = NEA.fromNonEmpty (h :| Array.fromFoldable t)
  where
  h :| t = unwrap a

isResumable :: T.ScrollState -> Boolean
isResumable T.Paused = true
isResumable T.YourError = true
isResumable T.OurError = true
isResumable _ = false

launchScroll :: LaunchScrollSig
launchScroll
  { cursor
  , nea
  , setCursor
  , newWagPush
  , setScrollState
  , setStopScrolling
  } = do
  pg <- new cursor
  stopScrolling <- subscribe
    (loopEmitter (_.duration >>> unwrap >>> round) (cursor + 1) $ unwrap nea)
    \{ wag } -> do
      pg' <- read pg
      let np = pg' + 1
      setCursor np
      write np pg
      newWagPush wag
  setScrollState T.Scrolling
  setStopScrolling stopScrolling

playScroll :: PlayScrollSig
playScroll
  { cursor
  , setCursor
  , scrollState
  , setScrollState
  , setStopScrolling
  , newWagPush
  , audioContext
  , compileOnPlay
  , bufferCache
  , currentPlaylist
  } =
  let
    nea' = nel2nea currentPlaylist
  in
    when (isResumable scrollState) $ launchAff_ do
      -- Log.info "playing scroll"
      for_ compileOnPlay (liftEffect <<< _.cleanErrorState)
      nea__ <- (map <<< map) (al <<< NEA.toNonEmpty) $
        compileOnPlay # maybe (pure (Just nea'))
          \{ code
           , setCurrentPlaylist
           , ourFaultErrorCallback
           , yourFaultErrorCallback
           , loader
           } -> makeAff \cb -> do
            setScrollState T.Loading
            compile
              { code: sanitizePS code -- sanitize in case there has been copy and paste
              , loader
              , compileUrl
              , ourFaultErrorCallback: \err -> do
                  ourFaultErrorCallback err
                  cb $ Left err
              , yourFaultErrorCallback: \err -> do
                  -- Log.info "executing yfec"
                  yourFaultErrorCallback err
                  cb $ Right Nothing
              , successCallback: \{ js } -> do
                  wag' <- liftEffect $ evalSources js
                    >>= runExceptT <<< readProp "wag"
                    >>= either (throwError <<< error <<< show) pure
                  let wag = (unsafeCoerce :: Foreign -> AFuture) wag'
                  let
                    newNea = fromMaybe nea' $ NEA.modifyAt (cursor `mod` NEA.length nea')
                      (_ { wag = wag, code = code })
                      nea'
                  launchAff_ do
                    res <- try do
                      doDownloads' audioContext bufferCache mempty identity wag
                      let newNel = nea2nel newNea
                      liftEffect $ setCurrentPlaylist $ newNel
                      liftEffect $ cb $ Right (Just newNea)
                    case res of
                      Left err -> do
                        liftEffect do
                          ourFaultErrorCallback err
                          setScrollState T.Paused
                          cb $ Left err
                      Right x -> pure x
              }
            mempty
      for_ nea__ \nea -> do
        -- Log.info "Starting scroll again"
        -- we move the cursor back one tick if we are compiling
        -- as we want to stay on that example a while and hear
        -- our beautiful creation!!
        let effectiveCursor = cursor - (maybe 0 (const 1) compileOnPlay)
        liftEffect $ launchScroll
          { cursor: effectiveCursor
          , nea: wrap nea
          , setCursor
          , newWagPush
          , setScrollState
          , setStopScrolling
          }

playWags :: PlayWagsSig
playWags
  { cursor
  , stopScrolling
  , setStopScrolling
  , setCursor
  , setScrollState
  , setNewWagPush
  , isPlaying
  , setIsPlaying
  , bufferCache
  , setTidalRes
  , setStopWags
  , currentPlaylist
  , setAudioContext
  } =
  when (not isPlaying) do
    -- Log.info "playing wags"
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
    ffiAudio <- liftEffect $ makeFFIAudioSnapshot audioCtx
    stopScrolling
    { event, push } <- create
    launchAff_ do
      rf <- liftEffect $ Ref.new (openFuture (wrap 1.0))
      usub0 <- liftEffect $ subscribe event \e -> Ref.write e rf
      when (waStatus /= "running") (toAffE $ contextResume audioCtx)
      map fold $ parTraverse
        (doDownloads' audioCtx bufferCache (pure $ pure unit) identity)
        (map _.wag currentPlaylist)
      let FullSceneBuilder { triggerWorld, piece } = engine (pure unit) (map (const <<< const) (r2b rf)) (pure {floats: V.fill (const 0.0)}) (pure (wrap mempty)) $ (Left (readableToBehavior bufferCache.read))
      trigger /\ world <- snd $ triggerWorld (audioCtx /\ (pure (pure {} /\ pure {})))
      unsub <- liftEffect $ subscribe
        (run trigger world { easingAlgorithm } (ffiAudio) piece)
        (\({ res } :: BehavingRun TidalRes Analysers) -> setTidalRes res)
      liftEffect do
        setNewWagPush push
        setAudioContext audioCtx
        setStopWags do
          unsub
          usub0
          close audioCtx
        playScroll
          { cursor
          -- no compile as we are restarting
          , compileOnPlay: Nothing
          , setCursor
          , audioContext: audioCtx
          , bufferCache
          , scrollState: T.Paused
          , setScrollState
          , setStopScrolling
          , newWagPush: push
          , currentPlaylist
          }

initialSampleCache :: SampleCache
initialSampleCache = Object.empty
