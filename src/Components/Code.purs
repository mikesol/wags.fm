module Components.Code where

import Prelude

import CSS (CSS, TimingFunction(..), animation, display, displayNone, forwards, fromString, iterationCount, left, normalAnimationDirection, pct, sec)
import Data.Array (intercalate)
import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
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
import Nonbili.DOM (innerText)
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

type Slots :: forall k. Row k
type Slots = ()

component :: forall m. MonadEffect m => H.Component T.CodeQuery T.CodeInput T.CodeOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< inj (Proxy :: _ "input")
        }
    }
  where
  initialState
    { pos
    , cursor
    , playlist
    , jump
    , scrollState
    } =
    { pos
    , cursor
    , playlist
    , scrollState
    , jump
    , lastQueriedCode: Nothing
    }

  render i = someCode
    where
    someCode =
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
            ( [ classes [ "language-purescript" ]
              ] <> case i.scrollState of
                T.YourError -> []
                T.OurError -> []
                _ ->
                  [ HE.onClick $ const
                      (inj (Proxy :: _ "pauseScroll") unit)
                  ]
            )
            [ HH.code
                [ HP.attr (H.AttrName "contenteditable")
                    ( case i.scrollState of
                        T.Loading -> "false"
                        _ -> "true"
                    )
                , classes [ "language-purescript" ]
                , HP.ref (H.RefLabel $ "code")
                ]
                [ HH.text $ fromMaybe' (\_ -> asMain (nelmod i.playlist.sequence i.jump).code)
                    ( case i.scrollState of
                        T.YourError -> i.lastQueriedCode
                        T.OurError -> i.lastQueriedCode
                        _ -> Nothing
                    )
                ]
            ]
        ]
      where
      cMod = (i.cursor + i.pos) `mod` 4

  handleQuery :: forall a. T.CodeQuery a -> HalogenM T.CodeState T.CodeAction Slots T.CodeOutput m (Maybe a)
  handleQuery = unwrap >>> VF.match
    { getCode: \f -> do
        mbc <- H.getHTMLElementRef (H.RefLabel $ "code")
        mbc # maybe (pure Nothing) \c -> do
          itxt <- H.liftEffect $ innerText c
          H.modify_ _ { lastQueriedCode = Just itxt }
          pure (Just (f itxt))
    }

  handleAction
    :: T.CodeAction
    -> HalogenM T.CodeState T.CodeAction Slots T.CodeOutput m Unit
  handleAction = match
    { input:
        \{ pos
         , cursor
         , jump
         , playlist
         , scrollState
         } -> do
          Log.info ("Receiving pos: " <> show pos <> " cursor: " <> show cursor)
          H.modify_ _
            { pos = pos
            , cursor = cursor
            , jump = jump
            , playlist = playlist
            , scrollState = scrollState
            }
    , pauseScroll: const do
        H.raise (inj (Proxy :: _ "pauseScroll") unit)
    }
