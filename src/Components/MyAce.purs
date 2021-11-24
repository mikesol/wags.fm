module Components.MyAce
  ( Slot
  , toStringMarkerType
  , component
  ) where

import Prelude

import Ace (Range)
import Ace as Ace
import Ace.EditSession as EditSession
import Ace.Editor as Edit
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types (Editor)
import Ace.VirtualRenderer as Renderer
import Data.Foldable (for_, traverse_)
import Data.Functor.Variant as VF
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Log
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Subscription as HS
import JIT.API (ErrorPosition)
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes)
import Web.HTML (HTMLElement)

type Slot = H.Slot T.MyAceQuery T.MyAceOutput

toStringMarkerType :: T.MarkerType -> String
toStringMarkerType = case _ of
  T.MarkerError -> "error"
  T.MarkerWarning -> "warning"

newtype MarkerId = MarkerId Int

derive instance newtypeMarkerId :: Newtype MarkerId _

debounceTime :: Milliseconds
debounceTime = Milliseconds 750.0

type State =
  { editor :: Maybe Editor
  , markers :: List MarkerId
  , unsubscribeFromHalogen :: Maybe SubscriptionId
  }

component :: forall i m. MonadAff m => Int -> H.Component T.MyAceQuery i T.MyAceOutput m
component id = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just (V.inj (Proxy :: _ "initialize") unit)
      , finalize = Just (V.inj (Proxy :: _ "finalize") unit)
      }
  }
  where
  initialState :: i -> State
  initialState _ =
    { editor: Nothing
    , markers: Nil
    , unsubscribeFromHalogen: Nothing
    }

  -- As we're embedding a 3rd party component we only need to create a placeholder
  -- div here and attach the ref property which will let us reference the element.
  render :: State -> H.ComponentHTML T.MyAceAction () m
  render _ =
    HH.div
      [ HP.ref $ H.RefLabel "ace"
      , HP.id ("code" <> show id)
      , classes ["w-full", "h-full"]
      , HE.onClick \_ -> (V.inj (Proxy :: _ "pauseScroll") unit)
      ]
      []

  handleAction :: T.MyAceAction -> H.HalogenM State T.MyAceAction () T.MyAceOutput m Unit
  handleAction = V.match
    { initialize: \_ ->
        do
          H.getHTMLElementRef (H.RefLabel "ace") >>= traverse_ \element -> do
            editor <- H.liftEffect $ setupEditor element
            { emitter, listener } <- H.liftEffect $ HS.create
            -- todo: call on finalize
            unsubscribeFromHalogen <- H.subscribe emitter
            H.modify_ _ { unsubscribeFromHalogen = Just unsubscribeFromHalogen, editor = Just editor }
            session <- H.liftEffect $ Editor.getSession editor
            H.liftEffect do
              emit <- debounce debounceTime \_ -> HS.notify listener (V.inj (Proxy :: _ "handleChange") unit)
              EditSession.onChange session emit
              pure unit

    , finalize: \_ ->
        do
          handleAction (V.inj (Proxy :: _ "clearMarkers") unit)
          H.modify_ _ { editor = Nothing }

    , clearMarkers: \_ ->
        do
          { editor: mbEditor, markers } <- H.get
          for_ mbEditor \editor -> H.liftEffect do
            session <- Editor.getSession editor
            let cleanup (MarkerId n) = EditSession.removeMarker n session
            traverse_ cleanup markers
    , pauseScroll: \_ -> H.raise (V.inj (Proxy :: _ "pauseScroll") unit)
    , handleChange: \_ -> do
        H.gets _.editor >>= traverse_ \editor -> do
          text <- H.liftEffect (Editor.getValue editor)
          H.raise $ (V.inj (Proxy :: _ "textChanged") text)
    }

  handleQuery :: forall a. T.MyAceQuery a -> H.HalogenM State T.MyAceAction () T.MyAceOutput m (Maybe a)
  handleQuery = unwrap >>> VF.match
    { getEditorContent: \reply -> do
        contents <- H.gets _.editor
          >>= traverse (Editor.getValue >>> H.liftEffect)
        pure (Just (reply contents))

    , setEditorContent: \(Tuple text next) -> do
        Log.info ("Setting content " <> text)
        H.gets _.editor >>= traverse_ \editor -> H.liftEffect do
          current <- Editor.getValue editor
          when (text /= current) do
            session <- Edit.getSession editor
            EditSession.setValue text session
        pure (Just next)

    , setAnnotations: \(Tuple annotations next) -> do
        H.gets _.editor >>= traverse_ \editor -> H.liftEffect do
          session <- Editor.getSession editor
          EditSession.setAnnotations annotations session
        pure (Just next)

    , addMarker: \(Tuple { markerType, position } next) -> do
        H.gets _.editor >>= traverse_ \editor -> do
          markerId <- H.liftEffect do
            session <- Editor.getSession editor
            range <- rangeFromPosition position
            let mt = toStringMarkerType markerType
            EditSession.addMarker range mt "text" true session
          H.modify_ \st -> st { markers = Cons (MarkerId markerId) st.markers }
        pure (Just next)

    , removeMarkers: \(Identity next) -> do
        handleAction (V.inj (Proxy :: _ "clearMarkers") unit)
        pure (Just next)
    }

setupEditor :: HTMLElement -> Effect Editor
setupEditor element = do
  editor <- Ace.editNode element Ace.ace
  Editor.setShowPrintMargin false editor
  Editor.setTheme "ace/theme/cobalt" editor

  renderer <- Editor.getRenderer editor
  Renderer.setShowGutter true renderer

  session <- H.liftEffect $ Editor.getSession editor
  EditSession.setMode "ace/mode/haskell" session
  EditSession.setTabSize 2 session
  EditSession.setUseSoftTabs true session

  pure editor

rangeFromPosition :: ErrorPosition -> Effect Range
rangeFromPosition pos = do
  let
    -- Ensure ranges are at least one character wide
    { startLine, startColumn, endLine, endColumn } =
      if pos.startLine == pos.endLine && pos.endColumn <= pos.startColumn then
        if pos.startColumn > 0 then do
          pos { startColumn = pos.endColumn - 1 }
        else
          pos { endColumn = pos.startColumn + 1 }
      else
        pos

  Range.create
    (startLine - 1)
    (startColumn - 1)
    (endLine - 1)
    (endColumn - 1)

debounce :: forall a. Milliseconds -> (a -> Effect Unit) -> Effect (a -> Effect Unit)
debounce (Milliseconds wait) k = do
  tidRef <- Ref.new Nothing
  pure \a -> do
    Ref.read tidRef >>= traverse_ clearTimeout
    tid <- setTimeout (Int.floor wait) do
      Ref.write Nothing tidRef
      k a
    Ref.write (Just tid) tidRef