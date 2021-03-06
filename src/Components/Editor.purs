module Components.Editor where

import Prelude

import CSS (CSS, TimingFunction(..), display, displayNone, fromString, left, ms, pct, sec)
import CSS.Hack.Animation (AnimationPlayState(..), animation, forwards, iterationCount, normalAnimationDirection)
import CSS.MyStyles (spin)
import Components.ErrorModal as EM
import Components.MyAce as MyAce
import Control.Plus (empty)
import Data.Array (intercalate, (..))
import Data.Foldable (traverse_)
import Data.Functor.Variant as VF
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, match)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Log
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import JIT.API as API
import SVGIcons as SVGIcons
import Svg.Renderer.Halogen (icon)
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes, classesS, nelmod, sanitizePS)

showPlayer
  :: forall r
   . Variant (showPlayer :: { transition :: { duration :: Number, offset :: Number } } | r)
showPlayer = inj (Proxy :: _ "showPlayer") { transition: { duration: 0.6, offset: 0.0 } }

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

editorClasses = [ "absolute", "w-full", "h-full" ] :: Array String

flyIn :: CSS
flyIn = animation
  (fromString "flyIn")
  (sec 1.0)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards
  ARunning

flyOut :: CSS
flyOut = animation
  (fromString "flyOut")
  (sec 1.0)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards
  ARunning

shuffle :: CSS
shuffle = left (pct 200.0)

panic :: CSS
panic = display displayNone

type Slots =
  ( modal :: forall query. H.Slot query T.ModalOutput Unit
  , code :: H.Slot T.MyAceQuery T.MyAceOutput Int
  )

compileErrorsToString :: Array API.CompilerError -> String
compileErrorsToString = intercalate "\n" <<< map \err ->
  maybe "" (\position -> "On line " <> show position.startLine <> ":\n") (toMaybe err.position)
    <> err.message
    <> "\n\n"

mjump :: Int -> Int -> Int
mjump cursor pos = (((cursor + (pos + 1)) / 4) * 4) - pos

component :: forall q m. MonadAff m => H.Component q T.EditorInput T.EditorOutput m
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

  render i@{ scrollState, playlist, cursor, modalShowable } =
    HH.div
      [ classes
          [ "w-full"
          , "h-full"
          , "absolute"
          , "grid"
          , "grid-rows-5"
          , "grid-cols-5"
          , "overflow-x-hidden"
          ]
      ] $
      [ HH.div
          [ classes
              [ "row-start-1"
              , "row-end-1"
              , "col-start-1"
              , "col-end-4"
              , if NEL.length playlist.sequence == 1 then "md:col-end-4" else "md:col-end-1"
              ]
          ]
          $
            if NEL.length playlist.sequence == 1 then
              [ HH.div [ classes [ "p-3" ] ]
                  [ HH.p_
                      [ HH.text "Looking for inspiration? You can copy and paste any example from the "
                      , HH.a
                          [ classes
                              [ "underline"
                              , "cursor-pointer"
                              , "flex-grow-0"
                              ]
                          , HP.target "_blank"
                          , HP.href "https://github.com/mikesol/wagsi/tree/main/cookbook"
                          ]
                          [ HH.text "cookbook" ]
                      , HH.text "."
                      ]
                  ]
              ]
            else
              [ HH.div [ classesS "w-full p-3" ]
                  [ HH.div [ classesS "overflow-hidden h-2 mb-2 text-xs flex rounded bg-pink-200" ]
                      [ HH.div
                          [ classesS "shadow-none flex flex-col text-center whitespace-nowrap text-white justify-center bg-pink-500"
                          , CSS.style do
                              animation
                                (fromString ("creepLeft" <> show (cursor `mod` 2)))
                                (ms $ ((unwrap (nelmod playlist.sequence cursor).duration)))
                                Linear
                                (sec 0.0)
                                (iterationCount 1.0)
                                normalAnimationDirection
                                forwards
                                case scrollState of
                                  T.Scrolling -> ARunning
                                  T.Loading -> AInitial
                                  _ -> APaused
                          ]
                          []
                      ]
                  , HH.div [ classes [ "text-pink-600" ] ]
                      let
                        plen = NEL.length playlist.sequence
                      in
                        intercalate [ HH.span_ [ HH.text " " ] ] $ map
                          ( \ix ->
                              [ HH.span
                                  ( let
                                      -- for now, we keep in place an
                                      -- imperfect hack by which we pause
                                      -- the scroll if the cursor is current
                                      -- even though this is not great UX, it saves us from having to kick off the animation again, which is hard in CSS when there is no reset first
                                      current = ix == cursor
                                    in
                                      [ classes [ "cursor-pointer" ]
                                      , HE.onClick $ const $ if current then inj (Proxy :: _ "pauseScroll") unit else inj (Proxy :: _ "moveCursorTo") ix
                                      ]
                                  )
                                  [ HH.text $ if cursor `mod` plen >= ix then "???" else "???" ]
                              ]
                          )
                          (0 .. (plen - 1))
                  ]
              ]
      , HH.div
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
                    , "flex-grow-0"
                    ]
              , HE.onClick $ const $ showPlayer
              ]
              [ HH.text "Back" ]
          ]
      , HH.div
          [ classes
              [ "row-start-5"
              , "row-end-5"
              , "col-start-1"
              , "col-end-3"
              , "flex"
              , "flex-col"
              ]
          ]
          [ HH.div [ classes [ "flex-grow" ] ] []
          , HH.div [ classes [ "flex-grow-0", "p-3" ] ]
              [ HH.span_
                  [ HH.text "Need help hacking? Check out these "
                  , HH.a
                      [ classes
                          [ "underline"
                          , "cursor-pointer"
                          ]
                      , HP.target "_blank"
                      , HP.href "https://mikesol.github.io/sf/crash-course"
                      ]
                      [ HH.text "docs" ]
                  , HH.text "."
                  ]
              ]
          ]
      , HH.div
          [ classes
              [ "row-start-2"
              , "row-end-6"
              , "col-start-1"
              , "col-end-6"
              , "md:row-start-2"
              , "md:row-end-5"
              , "md:col-start-2"
              , "md:col-end-5"
              , "flex"
              , "flex-col"
              ]
          ]
          ( [ HH.div [ classes [ "relative", "flex-grow" ] ]
                (map mkEditor (0 .. if NEL.length playlist.sequence == 1 then 0 else 3))
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
                    , case scrollState of
                        T.Scrolling -> HH.div
                          [ classes

                              [ "flex-grow-0"
                              , "cursor-pointer"
                              , "p-3"
                              ]
                          , HE.onClick $ const $ pauseScroll
                          ]
                          [ HH.text "Click the editor to change the sound."
                          ]
                        T.Paused -> HH.div
                          [ classes [ "flex-grow-0", "cursor-pointer", "p-3" ] ]
                          [ icon
                              ( (SVGIcons.playSolid 40 40)
                              )
                              [ HE.onClick $ const $ resumeScroll ]
                          ]
                        T.YourError -> HH.div
                          [ classes [ "flex-grow-0", "cursor-pointer", "p-3" ] ]
                          [ icon
                              ( (SVGIcons.playSolid 40 40)
                              )
                              [ HE.onClick $ const $ resumeScroll ]
                          ]
                        T.OurError -> HH.div
                          [ classes [ "flex-grow-0", "cursor-pointer", "p-3" ] ]
                          [ icon
                              (SVGIcons.playSolid 40 40)

                              [ HE.onClick $ const $ resumeScroll ]
                          ]
                        T.Loading -> HH.div
                          [ classes [ "flex-grow-0", "cursor-pointer", "p-3" ] ]
                          [ icon
                              ( ( SVGIcons.spinner
                                ) 40 40
                              )
                              [ CSS.style do
                                  spin
                              ]
                          ]
                    , HH.div [ classes [ "flex-grow" ] ] []
                    ]
                ]
          )
      ] <>
        ( modalShowable # maybe [] \ipt ->
            [ HH.slot (Proxy :: _ "modal") unit EM.component
                ipt
                (inj (Proxy :: _ "handleModalOutput"))
            ]
        )
    where
    mkEditor :: Int -> HH.HTML (H.ComponentSlot Slots m T.EditorAction) T.EditorAction
    mkEditor pos =
      let
        cMod = (i.cursor + pos) `mod` 4
      in
        HH.div
          [ classes editorClasses
          , CSS.style do
              if NEL.length playlist.sequence == 1 then
                pure unit
              else case i.cursor of
                0 -> case pos of
                  0 -> pure unit
                  1 -> left (pct (-150.0))
                  2 -> left (pct (200.0))
                  3 -> left (pct (200.0))
                  _ -> panic
                _ -> case cMod of
                  0 -> flyIn
                  1 -> flyOut
                  2 -> shuffle
                  3 -> shuffle
                  _ -> panic
          ]
          [ HH.slot (Proxy :: _ "code")
              pos
              (MyAce.component pos)
              unit
              (inj (Proxy :: _ "handleCodeOutput"))
          ]

  handleAction
    :: T.EditorAction
    -> HalogenM T.EditorState T.EditorAction Slots T.EditorOutput m Unit
  handleAction = match
    { input: \{ cursor, playlist, scrollState } -> do
        prevCursor <- H.gets _.cursor
        -- only set content when there is a cursor change
        -- which means that scroll is imminent
        -- as content will only change for something off screen
        -- there should be no flicker
        -- if there is, revisit!
        when (prevCursor /= cursor) $
          if
            -- if there is only one playlist, we don't advance it
            NEL.length playlist.sequence == 1 then do
            when (cursor == 0) do
              --Log.info $ "Seeting code" <> (sanitizePS (nelmod playlist.sequence 0).code)
              _ <- H.request
                (Proxy :: _ "code")
                0
                ( const $ T.MyAceQuery
                    $ VF.inj (Proxy :: _ "setEditorContent")
                    $ Tuple (sanitizePS (nelmod playlist.sequence 0).code) unit
                )
              mempty
          else do
            _ <- (0 .. 3) # traverse \pos -> do
              let jump = mjump cursor pos
              H.request
                (Proxy :: _ "code")
                pos
                ( const $ T.MyAceQuery
                    $ VF.inj (Proxy :: _ "setEditorContent")
                    $ Tuple (sanitizePS (nelmod playlist.sequence jump).code) unit
                )
            mempty
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
    , handleCodeOutput: match
        { textChanged: \_ -> do
            mempty
        , pauseScroll: \_ ->
            H.raise $ inj (Proxy :: _ "pauseScroll") unit
        }
    , moveCursorTo: H.raise
        <<< inj (Proxy :: _ "moveCursorTo")
        <<< (_ - 1)
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
        -- Log.info "Raising compile error"
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
        { cursor, listener, playlist } <- H.get
        code <- map join $ H.request (Proxy :: _ "code")
          (if NEL.length playlist.sequence == 1 then 0 else cursor)
          (T.MyAceQuery <<< VF.inj (Proxy :: _ "getEditorContent"))
        -- Log.info (code # maybe "did not get code" (append "got code: "))
        code # traverse_
          ( H.raise <<< playScroll <<<
              { code: _
              , ourFaultErrorCallback: \e -> do
                  Log.error (show e)
                  listener $ inj (Proxy :: _ "somethingWentWrong") unit
              , yourFaultErrorCallback: \e -> do
                  -- Log.info "actually calling yfec"
                  listener $ inj (Proxy :: _ "setMostRecentCompileErrors") e
              }
          )
    }
