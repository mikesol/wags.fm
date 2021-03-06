module Main where

import Prelude

import Components.Editor as Editor
import Components.Player as Player
import Control.Controller as C
import Control.Plus (empty)
import Data.Foldable (for_)
import Foreign.Object as Object
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Variant (inj, match)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (HalogenM)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import JIT.Compile (compile)
import JIT.Loader (makeLoader, Loader)
import Playlists.Java as Java
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes)
import WAGS.Lib.Tidal.Types (SampleCache)

_player :: Proxy "player"
_player = Proxy

_editor :: Proxy "editor"
_editor = Proxy

type Slots =
  ( player :: forall query. H.Slot query T.PlayerOutput Unit
  , editor :: forall query. H.Slot query T.EditorOutput Unit
  )

foreign import hackishlyRemoveInitialSSR :: Effect Unit

component :: forall q i o m. Loader -> MonadAff m => H.Component q i o m
component loader =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just (inj (Proxy :: _ "initialize") unit)
        }
    }
  where
  initialState _ =
    { playlist: Java.playlist
    , cursor: -1
    , playerTransition: { duration: 0.6, offset: 0.0 }
    , stopWags: mempty
    , stopScrolling: mempty
    , scrollState: T.Paused
    , isPlaying: empty
    , newWagPush: mempty
    , currentTidalRes: empty
    , bufferCache: empty
    , unsubscribeFromHalogen: empty
    , playerIsHidden: false
    , listener: mempty
    , audioContext: empty
    }

  render
    { cursor
    , playlist
    , isPlaying
    , playerTransition
    , playerIsHidden
    , scrollState
    } =
    HH.div [ classes [ "w-screen", "h-screen" ] ]
      [ HH.slot _editor unit Editor.component
          { cursor
          , playlist
          , scrollState
          }
          (inj (Proxy :: _ "handleEditorOutput"))
      , HH.slot _player unit Player.component
          { playlist
          , isPlaying
          , hiddenInstr:
              { hidden: playerIsHidden
              , transition: playerTransition
              }
          }
          (inj (Proxy :: _ "handlePlayerOutput"))
      ]

  handleAction
    :: T.MainAction
    -> HalogenM T.MainState T.MainAction Slots o m Unit
  handleAction = match
    { handleEditorOutput: match
        { showPlayer: \{ transition } -> do
            H.modify_ _
              { playerIsHidden = false
              , playerTransition = transition
              }
        , moveCursorTo: \cursor -> do
            { listener
            , newWagPush
            , playlist
            , stopScrolling
            } <- H.get
            H.liftEffect $ C.moveCursor
              { cursor
              , setCursor: listener <<< inj (Proxy :: _ "setCursor")
              , setScrollState: listener <<< inj (Proxy :: _ "setScrollState")
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , newWagPush
              , stopScrolling
              , nea: playlist.sequence
              }
        , playScroll:
            \{ code
             , ourFaultErrorCallback
             , yourFaultErrorCallback
             } -> do
              { cursor
              , listener
              , newWagPush
              , scrollState
              , playlist
              , audioContext
              } <- H.get
              bufferCache <- H.get >>= _.bufferCache >>> maybe (H.liftEffect (Ref.new Object.empty)) pure
              -- Log.info ("Setting yfec" <> show scrollState)
              for_ audioContext \ctx -> H.liftEffect $ C.playScroll
                { cursor
                , setCursor: listener <<< inj (Proxy :: _ "setCursor")
                , scrollState
                , setScrollState: listener <<< inj (Proxy :: _ "setScrollState")
                , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
                , newWagPush
                , audioContext: ctx
                , compileOnPlay: Just
                    { code
                    , loader
                    , cleanErrorState: mempty
                    , setCurrentPlaylist: listener <<< inj (Proxy :: _ "setCurrentPlaylist")
                    , ourFaultErrorCallback
                    , yourFaultErrorCallback
                    }
                , bufferCache:
                    { read: Ref.read bufferCache
                    , write: flip Ref.write bufferCache
                    }
                , currentPlaylist: playlist.sequence
                }
        , pauseScroll: const do
            { listener, stopScrolling } <- H.get
            H.liftEffect $ C.pauseScroll
              { stopScrolling
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , setScrollState: listener <<< inj (Proxy :: _ "setScrollState")
              }
        , editorInErrorState: const do
            H.modify_ _ { scrollState = T.OurError }
        , editorReceivedCompileError: const do
            -- Log.info "Got compile error"
            H.modify_ _ { scrollState = T.YourError }
        }
    , handlePlayerOutput: match
        { pressPlay: const do
            { cursor, listener, stopScrolling, isPlaying, playlist } <- H.get
            bufferCache <- H.get >>=
              _.bufferCache >>>
                maybe (H.liftEffect (Ref.new Object.empty)) pure
            H.liftEffect $ C.playWags
              { cursor
              , setTidalRes: listener <<< inj (Proxy :: _ "setTidalRes")
              , stopScrolling
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , setCursor: listener <<< inj (Proxy :: _ "setCursor")
              , setNewWagPush: listener <<< inj (Proxy :: _ "setNewWagPush")
              , setScrollState: listener <<< inj (Proxy :: _ "setScrollState")
              , setAudioContext: listener <<< inj (Proxy :: _ "setAudioContext")
              , isPlaying: isJust isPlaying
              , bufferCache:
                  { read: Ref.read bufferCache
                  , write: flip Ref.modify_ bufferCache <<< Object.union
                  }
              , setIsPlaying: listener <<< inj (Proxy :: _ "setIsPlaying") <<< if _ then pure { hasSentEvents: false } else Nothing
              , setStopWags: listener <<< inj (Proxy :: _ "setStopWags")
              , currentPlaylist: playlist.sequence
              }
        , pressStop: const do
            { listener, stopScrolling, stopWags } <- H.get
            H.liftEffect $ C.stopWags
              { setScrollState: listener <<< inj (Proxy :: _ "setScrollState")
              , setIsPlaying: listener <<< inj (Proxy :: _ "setIsPlaying") <<< if _ then pure { hasSentEvents: false } else Nothing
              , stopScrolling
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , stopWags
              , setCursor: listener <<< inj (Proxy :: _ "setCursor")
              , setStopWags: listener <<< inj (Proxy :: _ "setStopWags")
              }
        , choosePlaylist: \playlist -> do
            H.modify_ _ { playlist = playlist }
        , hidePlayer: \{ transition } -> do
            H.liftEffect hackishlyRemoveInitialSSR
            H.modify_ _
              { playerIsHidden = true
              , playerTransition = transition
              }
        }
    , setTidalRes: \tidalRes -> do
        { isPlaying: prevIsPlaying } <- H.get
        H.modify_ \i -> i
          { currentTidalRes = Just tidalRes
          , isPlaying = i.isPlaying <#> const { hasSentEvents: true }
          }
        when (prevIsPlaying == Just { hasSentEvents: false }) do
          H.liftEffect hackishlyRemoveInitialSSR
          H.modify_ _
            { playerIsHidden = true
            , playerTransition = { duration: 1.5, offset: 0.0 }
            }
    , initialize: const do
        ------------- we initialize the whole application with a call to the compiler
        ------------- this makes future calls go way faster
        H.liftEffect do
          compile
            { code:
                """module Main where

    import WAGS.Lib.Tidal (AFuture)
    import WAGS.Lib.Tidal.Tidal (make, s)

    wag :: AFuture
    wag = make 1.0 { earth: s "bd" }"""
            , loader
            , compileUrl: C.compileUrl
            , ourFaultErrorCallback: mempty
            , yourFaultErrorCallback: mempty
            , successCallback: mempty
            }
        { emitter, listener } <- H.liftEffect $ HS.create
        bufferCache <- H.liftEffect $ Ref.new (Object.empty :: SampleCache)
        unsubscribeFromHalogen <- H.subscribe emitter
        H.modify_ _
          { unsubscribeFromHalogen = Just unsubscribeFromHalogen
          , bufferCache = Just bufferCache
          , listener = HS.notify listener
          }
    , setAudioContext: \audioContext -> do
        H.modify_ _ { audioContext = Just audioContext }
    , setCurrentPlaylist: \sequence -> do
        H.modify_ \i -> i { playlist = i.playlist { sequence = sequence } }
    , setCursor: \cursor -> do
        H.modify_ _ { cursor = cursor }
    , setIsPlaying: \isPlaying -> do
        H.modify_ _ { isPlaying = isPlaying }
    , setScrollState: \scrollState -> do
        H.modify_ _ { scrollState = scrollState }
    , setNewWagPush: \newWagPush -> do
        H.modify_ _ { newWagPush = newWagPush }
    , setStopScrolling: \stopScrolling -> do
        H.modify_ _ { stopScrolling = stopScrolling }
    , setStopWags: \stopWags -> do
        H.modify_ _ { stopWags = stopWags }
    }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  let loader = makeLoader C.loaderUrl
  runUI (component loader) unit body
