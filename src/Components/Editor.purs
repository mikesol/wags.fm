module Components.Editor where

import Prelude

import CSS (CSS, TimingFunction(..), animation, display, displayNone, forwards, fromString, iterationCount, left, normalAnimationDirection, pct, sec)
import DOM.HTML.Indexed as I
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
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

editorClasses = [ "absolute", "w-full" ] :: Array String

flyIn :: CSS
flyIn = animation
  (fromString "flyIn")
  (sec 1.0)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards

flyOut :: CSS
flyOut = animation
  (fromString "flyOut")
  (sec 1.0)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards

shuffle :: CSS
shuffle = left (pct 200.0)

panic :: CSS
panic = display displayNone

asMain :: String -> String
asMain = intercalate "\n"
  <<< map
    ( (if _ then _ else _)
        <$> (eq "module " <<< String.take 7)
        <*> (const "module Main where")
        <*> identity
    )
  <<< String.split (String.Pattern "\n")

component :: forall q m. MonadEffect m => H.Component q T.EditorInput T.EditorOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< inj (Proxy :: _ "input")
        }
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
            case cMod of
              0 -> flyIn
              1 -> flyOut
              2 -> shuffle
              3 -> shuffle
              _ -> panic
        ]
        [ HH.pre
            [ HE.onClick $ const
                (inj (Proxy :: _ "pauseScroll") unit)
            , classes [ "language-purescript" ]
            ]
            [ HH.code
                [ HP.attr (H.AttrName "contenteditable") "true"
                , classes [ "language-purescript" ]
                , HP.ref (H.RefLabel $ "code" <> show pos)
                ]
                [ HH.text $ asMain (nelmod i.playlist.sequence jump).code
                ]
            ]
        ]
      where
      jump = (((i.cursor + (pos + 1)) / 4) * 4) - pos
      cMod = (i.cursor + pos) `mod` 4

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
