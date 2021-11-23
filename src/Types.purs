module Types where

import Prelude

import Data.Functor.Variant (VariantF)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (SubscriptionId)
import JIT.API as API
import Type.Proxy (Proxy(..))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Types (SampleCache)
import WAGS.WebAPI (AudioContext)

type Cursor = Int
data ScrollState = Scrolling | Paused | Loading | YourError | OurError

type PlaylistSequence = NonEmptyList
  { duration :: Milliseconds
  , code :: String
  , wag :: AFuture
  }

type Playlist =
  { title :: String
  , sequence :: PlaylistSequence
  }

type CodeInput =
  { pos :: Int
  , cursor :: Int
  , playlist :: Playlist
  , scrollState :: ScrollState
  }
newtype CodeQuery a = CodeQuery (VariantF (getCode :: (->) String) a)
derive instance newtypeCodeQuery :: Newtype (CodeQuery a) _
type CodeOutput = Variant (pauseScroll :: Unit)
type CodeAction = Variant (pauseScroll :: Unit, input :: CodeInput)
type CodeState =
  { pos :: Int
  , cursor :: Int
  , playlist :: Playlist
  , scrollState :: ScrollState
  , lastQueriedCode :: Maybe String
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
  , handleCodeOutput :: CodeOutput
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
