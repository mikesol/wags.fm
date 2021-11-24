module Types where

import Prelude

import Ace as Ace
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Halogen.Query.HalogenM (SubscriptionId)
import JIT.API as API
import Type.Proxy (Proxy(..))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Types (SampleCache)
import WAGS.WebAPI (AudioContext)

type Cursor = Int
data ScrollState = Scrolling | Paused | Loading | YourError | OurError

derive instance genericScrollState :: Generic ScrollState _
instance showScrollState :: Show ScrollState where
  show = genericShow

type PlaylistSequence = NonEmptyList
  { duration :: Milliseconds
  , code :: String
  , wag :: AFuture
  }

type Playlist =
  { title :: String
  , sequence :: PlaylistSequence
  }

type EditorInput =
  { playlist :: Playlist
  , cursor :: Cursor
  , scrollState :: ScrollState
  }

type EditorState =
  { playlist :: Playlist
  , unsubscribeFromHalogen :: Maybe SubscriptionId
  , listener :: EditorAction -> Effect Unit
  , mostRecentCompileErrors :: Array API.CompilerError
  , cursor :: Cursor
  , modalShowable :: Maybe ModalInput
  , scrollState :: ScrollState
  }

type MainState =
  { playlist :: Playlist
  , listener :: MainAction -> Effect Unit
  , cursor :: Int
  , stopScrolling :: Effect Unit
  , scrollState :: ScrollState
  , isPlaying :: Boolean
  , bufferCache :: Maybe (Ref SampleCache)
  , unsubscribeFromHalogen :: Maybe SubscriptionId
  , playerIsHidden :: Boolean
  , audioContext :: Maybe AudioContext
  , stopWags :: Effect Unit
  , newWagPush :: AFuture -> Effect Unit
  }

type ModalAction = Variant (close :: Unit)
type ModalInput =
  { text :: String
  , title :: String
  , code :: Maybe String
  }

type ModalOutput = Variant (closeMe :: Unit)
type ModalState =
  { text :: String
  , title :: String
  , code :: Maybe String
  }

----
type EditorAction = Variant
  ( pauseScroll :: Unit
  , initialize :: Unit
  , setMostRecentCompileErrors :: Array API.CompilerError
  , showPlayer :: Unit
  , resumeScroll :: Unit
  , somethingWentWrong :: Unit
  , input :: EditorInput
  , showCompileError :: Unit
  , handleModalOutput :: ModalOutput
  , handleCodeOutput :: MyAceOutput
  )

type PlayScrollInfo =
  { code :: String
  , ourFaultErrorCallback :: Error -> Effect Unit
  , yourFaultErrorCallback :: Array API.CompilerError -> Effect Unit
  }

type EditorOutput = Variant
  ( playScroll :: PlayScrollInfo
  , editorInErrorState :: Unit
  , editorReceivedCompileError :: Unit
  , pauseScroll :: Unit
  , showPlayer :: Unit
  )

type MainAction = Variant
  ( initialize :: Unit
  , handlePlayerOutput :: PlayerOutput
  , setIsPlaying :: Boolean
  , setAudioContext :: AudioContext
  , setCursor :: Int
  , setCurrentPlaylist :: PlaylistSequence
  , handleEditorOutput :: EditorOutput
  , setScrollState :: ScrollState
  , setStopWags :: Effect Unit
  , setNewWagPush :: AFuture -> Effect Unit
  , setStopScrolling :: Effect Unit
  )

type PlayerAction = Variant
  ( input :: PlayerInput
  , choosePlaylist :: Playlist
  , pressPlay :: Unit
  , pressStop :: Unit
  , hidePlayer :: Unit
  )

type PlayerInput =
  { playlist :: Playlist
  , isPlaying :: Boolean
  , isHidden :: Boolean
  }

type PlayerOutput = Variant
  ( choosePlaylist :: Playlist
  , pressPlay :: Unit
  , pressStop :: Unit
  , hidePlayer :: Unit
  )

type PlayerState =
  { playlist :: Playlist
  , isHidden :: Boolean
  , isPlaying :: Boolean
  , hasPlayedOnce :: Boolean
  , hasHiddenOnce :: Boolean
  }

------
------
data MarkerType = MarkerError | MarkerWarning

derive instance eqMarkerType :: Eq MarkerType

type MarkerInfo =
  { markerType :: MarkerType
  , position :: API.ErrorPosition
  }

newtype MyAceQuery a = MyAceQuery
  ( VariantF
      ( getEditorContent :: (->) (Maybe String)
      , setEditorContent :: Tuple String
      , setAnnotations :: Tuple (Array Ace.Annotation)
      , addMarker :: Tuple MarkerInfo
      , removeMarkers :: Identity
      )
      a
  )

derive instance newtypeMyAceQuery :: Newtype (MyAceQuery a) _

_getEditorContent :: forall a. (Maybe String -> a) -> MyAceQuery a
_getEditorContent = MyAceQuery <<< VF.inj (Proxy :: _ "getEditorContent")

_setEditorContent :: forall a. Tuple String a -> MyAceQuery a
_setEditorContent = MyAceQuery <<< VF.inj (Proxy :: _ "setEditorContent")

_setAnnotations :: forall a. Tuple (Array Ace.Annotation) a -> MyAceQuery a
_setAnnotations = MyAceQuery <<< VF.inj (Proxy :: _ "setAnnotations")

_addMarker :: forall a. Tuple MarkerInfo a -> MyAceQuery a
_addMarker = MyAceQuery <<< VF.inj (Proxy :: _ "addMarker")

_removeMarkers :: forall a. Identity a -> MyAceQuery a
_removeMarkers = MyAceQuery <<< VF.inj (Proxy :: _ "removeMarkers")

type MyAceAction = Variant
  ( initialize :: Unit
  , finalize :: Unit
  , clearMarkers :: Unit
  , handleChange :: Unit
  , pauseScroll :: Unit
  )

type MyAceOutput = Variant (textChanged :: String, pauseScroll :: Unit)
