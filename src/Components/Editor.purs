module Components.Editor where

import Prelude

import DOM.HTML.Indexed as I
import Data.Variant (Variant, inj, match)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Nouns as N
import Type.Proxy (Proxy(..))
import Util (classes)
import Verbs as V

showPlayer
  :: forall r
   . Variant (showPlayer :: Unit | r)
showPlayer = inj (Proxy :: _ "showPlayer") unit

hilightCode :: forall w i. HH.Node (I.Interactive ()) w i
hilightCode = HH.element (HH.ElemName "deckgo-highlight-code")

component :: forall q m. H.Component q N.EditorInput V.EditorOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState { cursor, playlist } = { cursor, playlist }

  render _ =
    HH.div
      [ classes
          [ "w-full"
          , "h-full"
          , "absolute"
          , "grid"
          , "grid-rows-5"
          , "grid-cols-5"
          ]
      ]
      [ HH.div
          [ classes
              [ "row-start-1"
              , "row-end-1"
              , "col-start-5"
              , "col-end-5"
              , "flex"
              , "flex-row"
              ]
          ]
          [ HH.div [ classes [ "flex-grow" ] ] []
          , HH.a
              [ classes
                  $
                    [ "underline"
                    , "cursor-pointer"
                    , "p-3"
                    ]
              , HE.onClick $ const $ showPlayer
              ]
              [ HH.text "Back" ]
          ]
      , HH.div
          [ classes
              [ "row-start-2"
              , "row-end-5"
              , "col-start-2"
              , "col-end-5"
              ]
          ]
          [ hilightCode
              [ HP.attr (H.AttrName "language") "haskell"
              ]
              [ HH.code [ HP.attr (H.AttrName "slot") "code" ] [ HH.text "module foo where" ]
              ]
          ]
      ]

  handleAction
    :: V.EditorAction
    -> HalogenM N.EditorState V.EditorAction () V.EditorOutput m Unit
  handleAction = match
    { input: \{ cursor, playlist } ->
        do
          H.modify_ _ { cursor = cursor, playlist = playlist }
    , showPlayer: const $ do
        H.raise showPlayer
    }
