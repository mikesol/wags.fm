module Components.Editor where

import Prelude

import CSS (CSS, TimingFunction(..), animation, display, displayNone, forwards, fromString, iterationCount, left, normalAnimationDirection, pct, sec)
import DOM.HTML.Indexed as I
import Data.Foldable (for_, traverse_)
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Nonbili.DOM (innerText)
import SVGIcons as SVGIcons
import Svg.Renderer.Halogen (icon)
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes, nelmod)

showPlayer
  :: forall r
   . Variant (showPlayer :: Unit | r)
showPlayer = inj (Proxy :: _ "showPlayer") unit

resumeScroll
  :: forall r. Variant (resumeScroll :: Unit | r)
resumeScroll = inj (Proxy :: _ "resumeScroll") unit

playScroll
  :: forall r
   . String
  -> Variant (playScroll :: String | r)
playScroll = inj (Proxy :: _ "playScroll")

pauseScroll
  :: forall r
   . Variant (pauseScroll :: Unit | r)
pauseScroll = inj (Proxy :: _ "pauseScroll") unit

hilightCode :: forall w i. HH.Node (I.Interactive ()) w i
hilightCode = HH.element (HH.ElemName "deckgo-highlight-code")

editorClasses = [ "absolute", "w-full" ] :: Array String

flyIn :: CSS
flyIn = animation
  (fromString "flyIn")
  (sec 0.6)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards

flyOut :: CSS
flyOut = animation
  (fromString "flyIn")
  (sec 0.6)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards

shuffle :: CSS
shuffle = left (pct 200.0)

panic :: CSS
panic = display displayNone

{-
0 0 -> 0
0 1 -> 0
0 2 -> 0
0 3 -> 4
0 4 -> 4
0 5 -> 4
0 6 -> 4
0 7 -> 8
0 (((n+1) / 4) * 4) - 0
1 0 -> -1
1 1 -> -1
1 2 -> 3
1 3 -> 3
1 4 -> 3
1 5 -> 3
1 6 -> 7
1 7 -> 7
1 (((n+2) / 4) * 4) - 1
2 0 -> -2
2 1 -> 2
2 2 -> 2
2 (((n+3) / 4) * 4) - 2
-}

component :: forall q m. MonadEffect m => H.Component q T.EditorInput T.EditorOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState
    { cursor
    , playlist
    , isScrolling
    } =
    { cursor
    , playlist
    , isScrolling
    }

  render i@{ isScrolling } =
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
              , "flex"
              , "flex-col"
              ]
          ]
          [ HH.div [ classes [ "relative", "flex-grow" ] ]
              [ someCode 0
              , someCode 1
              , someCode 2
              , someCode 3
              ]
          , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
              [ HH.div [ classes [ "flex-grow" ] ] []
              , HH.div
                  [ classes [ "flex-grow-0", "cursor-pointer" ] ]
                  [ icon
                      ( ( if isScrolling then
                            SVGIcons.pauseSolid
                          else SVGIcons.playSolid
                        ) 50 50
                      )
                      [ HE.onClick $ const $
                          if isScrolling then pauseScroll else resumeScroll
                      ]
                  ]
              , HH.div [ classes [ "flex-grow" ] ] []
              ]
          ]
      ]
    where
    someCode pos =
      HH.div
        [ classes editorClasses
        , CSS.style do
            case (i.cursor + pos) `mod` 4 of
              0 -> flyIn
              1 -> flyOut
              2 -> shuffle
              3 -> shuffle
              _ -> panic
        ]
        [ hilightCode
            [ HP.attr (H.AttrName "language") "haskell"
            ]
            [ HH.code
                [ HP.attr (H.AttrName "slot") "code"
                , HP.ref (H.RefLabel $ "code" <> show pos)
                ]
                [ HH.text
                    (nelmod i.playlist.sequence jump).code
                ]
            ]
        ]
      where
      jump = (((i.cursor + (pos + 1)) / 4) * 4) - pos

  handleAction
    :: T.EditorAction
    -> HalogenM T.EditorState T.EditorAction () T.EditorOutput m Unit
  handleAction = match
    { input: \{ cursor, playlist, isScrolling } ->
        do
          H.modify_ _
            { cursor = cursor
            , isScrolling = isScrolling
            , playlist = playlist
            }
    , showPlayer: const do
        H.raise showPlayer
    , pauseScroll: const do
        H.raise pauseScroll
    , resumeScroll: const do
        { cursor } <- H.get
        H.getHTMLElementRef (H.RefLabel $ "code" <> show ((4 - (cursor `mod` 4)) `mod` 4))
          >>= traverse_ (H.liftEffect <<< innerText >=> H.raise <<< playScroll)
    }
