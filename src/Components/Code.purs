module Components.Code where

import Prelude

import CSS (CSS, TimingFunction(..), animation, display, displayNone, forwards, fromString, iterationCount, left, normalAnimationDirection, pct, sec)
import DOM.HTML.Indexed as I
import Data.Array (intercalate)
import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Variant (inj, match)
import Debug (spy)
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

hilightCode :: forall w i. HH.Node (I.Interactive ()) w i
hilightCode = HH.element (HH.ElemName "deckgo-highlight-code")

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
    , isInErrorState
    , jump
    , isEditable
    } =
    { pos
    , cursor
    , playlist
    , isEditable
    , isInErrorState
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
        [ hilightCode
            [ HP.attr (H.AttrName "language") "purescript"
            , HP.attr (H.AttrName "editable") (if i.isEditable then "true" else "false")
            , HE.onClick $ const (inj (Proxy :: _ "pauseScroll") unit)
            ]
            [ HH.code [ HP.attr (H.AttrName "slot") "code", HP.ref (H.RefLabel $ "code") ]
                [ HH.text $ fromMaybe' (\_ -> asMain (nelmod i.playlist.sequence i.jump).code)
                    (if i.isInErrorState then i.lastQueriedCode else Nothing)
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
         , isEditable
         , isInErrorState
         } -> do
          st <- H.get
          -- Log.info ("Receiving pos: " <> show pos <> " cursor: " <> show cursor)
          when
            ( (pos /= st.pos)
                || (cursor /= st.cursor)
                || (jump /= st.jump)
                || (isEditable /= st.isEditable) /= (isInErrorState /= st.isInErrorState)
                || ((nelmod playlist.sequence jump).code /= (nelmod st.playlist.sequence st.jump).code)
            )
            do
              Log.info "rendering"
              H.modify_ _
                { pos = pos
                , cursor = cursor
                , jump = jump
                , playlist = playlist
                , isEditable = isEditable
                , isInErrorState = isInErrorState
                }
    , pauseScroll: const do
        H.raise (inj (Proxy :: _ "pauseScroll") unit)
    }

{-HH.pre
( [ classes [ "language-purescript" ]
  ] <> case i.isEditable of
    T.YourError -> []
    T.OurError -> []
    _ ->
      [ HE.onClick $ const
          (inj (Proxy :: _ "pauseScroll") unit)
      ]
)
[ HH.code
    [ HP.attr (H.AttrName "contenteditable")
        ( case i.isEditable of
            T.Loading -> "false"
            _ -> "true"
        )
    , classes [ "language-purescript" ]
    , HP.ref (H.RefLabel $ "code")
    ]
    [ HH.text $ fromMaybe' (\_ -> asMain (nelmod i.playlist.sequence i.jump).code)
        ( case i.isEditable of
            T.YourError -> i.lastQueriedCode
            T.OurError -> i.lastQueriedCode
            _ -> Nothing
        )
    ]
]-}