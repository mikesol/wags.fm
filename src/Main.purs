module Main where

import Prelude

import Components.Editor as Editor
import Components.Player as Player
import Control.Controller as C
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (inj, match)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import Halogen (HalogenM)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
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

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
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
    , cursor: 0
    , stopWags: mempty
    , stopScrolling: mempty
    , isScrolling: false
    , isPlaying: false
    , newWagPush: mempty
    , bufferCache: Nothing
    , unsubscribeFromHalogen: Nothing
    , playerIsHidden: false
    , listener: mempty
    , audioContext: Nothing
    }

  render { cursor, playlist, isPlaying, playerIsHidden, isScrolling } =
    HH.div [ classes [ "w-screen", "h-screen" ] ]
      [ HH.slot _editor unit Editor.component
          { cursor
          , playlist
          , isScrolling
          }
          (inj (Proxy :: _ "handleEditorOutput"))
      , HH.slot _player unit Player.component
          { playlist
          , isPlaying
          , isHidden: playerIsHidden
          }
          (inj (Proxy :: _ "handlePlayerOutput"))
      ]

  handleAction
    :: T.MainAction
    -> HalogenM T.MainState T.MainAction Slots o m Unit
  handleAction = match
    { handleEditorOutput: match
        { showPlayer: const do
            H.modify_ _ { playerIsHidden = false }
        , playScroll: \code -> do
            { cursor
            , listener
            , newWagPush
            , isScrolling
            , playlist
            , audioContext
            } <- H.get
            bufferCache <- H.get >>= _.bufferCache >>> maybe (H.liftEffect (Ref.new Map.empty)) pure
            for_ audioContext \ctx -> H.liftEffect $ C.playScroll
              { cursor
              , setCursor: listener <<< inj (Proxy :: _ "setCursor")
              , isScrolling
              , setIsScrolling: listener <<< inj (Proxy :: _ "setIsScrolling")
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , newWagPush
              , audioContext: ctx
              , compileOnPlay: Just
                  { code
                  , cleanErrorState: mempty
                  , setCurrentPlaylist: listener <<< inj (Proxy :: _ "setCurrentPlaylist")
                  , ourFaultErrorCallback: mempty
                  , yourFaultErrorCallback: mempty
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
              , setIsScrolling: listener <<< inj (Proxy :: _ "setIsScrolling")
              }
        }
    , handlePlayerOutput: match
        { pressPlay: const do
            { cursor, listener, stopScrolling, isPlaying, playlist } <- H.get
            bufferCache <- H.get >>=
              _.bufferCache >>>
                maybe (H.liftEffect (Ref.new Map.empty)) pure
            H.liftEffect $ C.playWags
              { cursor
              , stopScrolling
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , setCursor: listener <<< inj (Proxy :: _ "setCursor")
              , setNewWagPush: listener <<< inj (Proxy :: _ "setNewWagPush")
              , setIsScrolling: listener <<< inj (Proxy :: _ "setIsScrolling")
              , setAudioContext: listener <<< inj (Proxy :: _ "setAudioContext")
              , isPlaying
              , bufferCache:
                  { read: Ref.read bufferCache
                  , write: flip Ref.write bufferCache
                  }
              , setIsPlaying: listener <<< inj (Proxy :: _ "setIsPlaying")
              , setStopWags: listener <<< inj (Proxy :: _ "setStopWags")
              , currentPlaylist: playlist.sequence
              }
        , pressStop: const do
            { listener, stopScrolling, stopWags } <- H.get
            H.liftEffect $ C.stopWags
              { setIsScrolling: listener <<< inj (Proxy :: _ "setIsScrolling")
              , setIsPlaying: listener <<< inj (Proxy :: _ "setIsPlaying")
              , stopScrolling
              , setStopScrolling: listener <<< inj (Proxy :: _ "setStopScrolling")
              , stopWags
              , setStopWags: listener <<< inj (Proxy :: _ "setStopWags")
              }
        , choosePlaylist: \playlist -> do
            H.modify_ _ { playlist = playlist }
        , hidePlayer: const do
            H.modify_ _ { playerIsHidden = true }
        }
    , initialize: const do
        { emitter, listener } <- H.liftEffect $ HS.create
        bufferCache <- H.liftEffect $ Ref.new (Map.empty :: SampleCache)
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
    , setIsScrolling: \isScrolling -> do
        H.modify_ _ { isScrolling = isScrolling }
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
  runUI component unit body
