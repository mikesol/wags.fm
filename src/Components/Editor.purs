module Components.Editor where

import Prelude

import CSS (CSS, TimingFunction(..), animation, display, displayNone, forwards, fromString, infinite, iterationCount, left, normalAnimationDirection, pct, sec)
import Components.ErrorModal as EM
import Control.Plus (empty)
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import JIT.API as API
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
   . T.PlayScrollInfo
  -> Variant (playScroll :: T.PlayScrollInfo | r)
playScroll = inj (Proxy :: _ "playScroll")

pauseScroll
  :: forall r
   . Variant (pauseScroll :: Unit | r)
pauseScroll = inj (Proxy :: _ "pauseScroll") unit

editorClasses = [ "absolute", "w-full" ] :: Array String

spin :: CSS
spin = animation
  (fromString "spin")
  (sec 1.2)
  Linear
  (sec 0.0)
  infinite
  normalAnimationDirection
  forwards

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

type Slots =
  ( modal :: forall query. H.Slot query T.ModalOutput Unit
  )

compileErrorsToString :: Array API.CompilerError -> String
compileErrorsToString = intercalate "\n" <<< map \err ->
  maybe "" (\position -> "On line " <> show position.startLine <> ":\n") (toMaybe err.position)
    <> err.message
    <> "\n\n"

component :: forall q m. MonadEffect m => H.Component q T.EditorInput T.EditorOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ inj (Proxy :: _ "initialize") unit
        , receive = Just <<< inj (Proxy :: _ "input")
        }
    }
  where
  initialState
    { cursor
    , playlist
    , scrollState
    } =
    { cursor
    , playlist
    , scrollState
    , listener: mempty
    , mostRecentCompileErrors: []
    , unsubscribeFromHalogen: empty
    , modalShowable: empty
    }

  render i@{ scrollState, modalShowable } =
    HH.div
      [ classes
          [ "w-full"
          , "h-full"
          , "absolute"
          , "grid"
          , "grid-rows-5"
          , "grid-cols-5"
          ]
      ] $
      ( modalShowable # maybe [] \ipt ->
          [ HH.slot (Proxy :: _ "modal") unit EM.component
              ipt
              (inj (Proxy :: _ "handleModalOutput"))
          ]
      ) <>
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
            ( [ HH.div [ classes [ "relative", "flex-grow" ] ]
                  [ someCode 0
                  , someCode 1
                  , someCode 2
                  , someCode 3
                  ]
              ]
                <>
                  ( case scrollState of
                      T.YourError ->
                        [ HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
                            [ HH.div [ classes [ "flex-grow" ] ] []
                            , HH.div [ classes [ "flex-grow-0" ] ]
                                [ HH.span []
                                    [ HH.text "The code above has errors."
                                    , HH.a
                                        [ classes
                                            [ "underline"
                                            , "cursor-pointer"
                                            , "p-3"
                                            ]
                                        , HE.onClick (const $ inj (Proxy :: _ "showCompileError") unit)
                                        ]
                                        [ HH.text "Show." ]
                                    ]
                                ]
                            ]
                        ]
                      _ -> []
                  )
                <>
                  [ HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
                      [ HH.div [ classes [ "flex-grow" ] ] []
                      , HH.div
                          [ classes [ "flex-grow-0", "cursor-pointer" ] ]
                          [ icon
                              ( ( case scrollState of
                                    T.Scrolling -> SVGIcons.pauseSolid
                                    T.Paused -> SVGIcons.playSolid
                                    T.YourError -> SVGIcons.playSolid
                                    T.OurError -> SVGIcons.playSolid
                                    T.Loading -> SVGIcons.spinner
                                ) 50 50
                              )
                              case scrollState of
                                T.Scrolling -> [ HE.onClick $ const $ pauseScroll ]
                                T.Paused -> [ HE.onClick $ const $ resumeScroll ]
                                T.YourError -> [ HE.onClick $ const $ resumeScroll ]
                                T.OurError -> [ HE.onClick $ const $ resumeScroll ]
                                T.Loading ->
                                  [ CSS.style do
                                      spin
                                  ]
                          ]
                      , HH.div [ classes [ "flex-grow" ] ] []
                      ]
                  ]
            )
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
                [ HP.attr (H.AttrName "contenteditable")
                    ( case scrollState of
                        T.Loading -> "false"
                        _ -> "true"
                    )
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
    -> HalogenM T.EditorState T.EditorAction Slots T.EditorOutput m Unit
  handleAction = match
    { input: \{ cursor, playlist, scrollState } ->
        do
          H.modify_ _
            { cursor = cursor
            , scrollState = scrollState
            , playlist = playlist
            }
    , initialize: const do
        { emitter, listener } <- H.liftEffect $ HS.create
        unsubscribeFromHalogen <- H.subscribe emitter
        H.modify_ _
          { unsubscribeFromHalogen = Just unsubscribeFromHalogen
          , listener = HS.notify listener
          }
    , showCompileError: const do
        { mostRecentCompileErrors } <- H.get
        H.modify_ _
          { modalShowable = Just
              { text: "Hey it happens. If I had a nickel for every time my code didn't compile, I wouldn't be building a web radio service. I wish you lots of nickels. Here's what went wrong."
              , title: "Compile errors"
              , code: Just (compileErrorsToString mostRecentCompileErrors)
              }
          }
    , handleModalOutput: match
        { closeMe: const do
            H.modify_ _
              { modalShowable = Nothing
              }
        }
    , showPlayer: const do
        H.raise showPlayer
    , pauseScroll: const do
        H.raise pauseScroll
    , setMostRecentCompileErrors: \mostRecentCompileErrors -> do
        Log.info "Raising compile error"
        H.raise (inj (Proxy :: _ "editorReceivedCompileError") unit)
        H.modify_ _
          { mostRecentCompileErrors = mostRecentCompileErrors
          }
    , somethingWentWrong: const do
        H.raise (inj (Proxy :: _ "editorInErrorState") unit)
        H.modify_ _
          { modalShowable = Just
              { text: "Something broke, and quite frankly, we don't know what. What now? Perhaps try what you were doing again. Perhaps not. Perhaps open the console and send us the error message. Mostly, we're sorry."
              , title: "Ohes noes!"
              , code: Nothing
              }
          }
    , resumeScroll: const do
        { cursor, listener } <- H.get
        H.getHTMLElementRef (H.RefLabel $ "code" <> show ((4 - (cursor `mod` 4)) `mod` 4))
          >>= traverse_
            ( H.liftEffect <<< innerText >=> H.raise <<< playScroll <<<
                { code: _
                , ourFaultErrorCallback: \e -> do
                    Log.error (show e)
                    listener $ inj (Proxy :: _ "somethingWentWrong") unit
                , yourFaultErrorCallback: \e -> do
                   Log.info "actually calling yfec"
                   listener $ inj (Proxy :: _ "setMostRecentCompileErrors") e
                }
            )
    }
