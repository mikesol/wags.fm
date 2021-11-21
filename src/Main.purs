module Main where

import Prelude

import Components.Editor as Editor
import Components.Player as Player
import Data.Maybe (Maybe(..))
import Data.Variant (inj, match)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Playlists.Java as Java
import Type.Proxy (Proxy(..))
import Util (classes)
import Verbs as V
import WAGS.WebAPI (AudioContext)

type Action = V.MainAction

_player :: Proxy "player"
_player = Proxy

_editor :: Proxy "editor"
_editor = Proxy

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { playlist: Java.playlist
    , cursor: 0
    , isScrolling: false
    , isPlaying: false
    , playerIsHidden: false
    , audioContext: Nothing :: Maybe AudioContext
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

  handleAction = match
    { handlePlayerOutput: match
        { pressPlay: const $ do
            H.modify_ _ { isPlaying = true, isScrolling = true }
        , pressStop: const $ do
            H.modify_ _ { isPlaying = false, isScrolling = false }
        , choosePlaylist: \playlist -> do
            H.modify_ _ { playlist = playlist }
        , hidePlayer: const $ do
            H.modify_ _ { playerIsHidden = true }
        }
    , handleEditorOutput: match
        { showPlayer: const $ do
            H.modify_ _ { playerIsHidden = false }
        , pressPlay: const $ do
            H.modify_ _ { isScrolling = true }
        , pressPause: const $ do
            H.modify_ _ { isScrolling = false }
        }
    }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
