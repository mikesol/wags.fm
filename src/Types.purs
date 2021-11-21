module Types where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Ref (Ref)
import Halogen.Query.HalogenM (SubscriptionId)
import Type.Function (type ($))
import Type.Row (type (+))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Types (SampleCache)
import WAGS.WebAPI (AudioContext)

-----------
-- nouns

type Cursor = Int

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
  , isScrolling :: Boolean
  }

type EditorState =
  { playlist :: Playlist
  , cursor :: Cursor
  , isScrolling :: Boolean
  }

type MainState =
  { playlist :: Playlist
  , listener :: MainAction -> Effect Unit
  , cursor :: Int
  , stopScrolling :: Effect Unit
  , isScrolling :: Boolean
  , isPlaying :: Boolean
  , bufferCache :: Maybe (Ref SampleCache)
  , unsubscribeFromHalogen :: Maybe SubscriptionId
  , playerIsHidden :: Boolean
  , audioContext :: Maybe AudioContext
  , stopWags :: Effect Unit
  , newWagPush :: AFuture -> Effect Unit
  }

type PlayerInput =
  { playlist :: Playlist
  , isPlaying :: Boolean
  , isHidden :: Boolean
  }

type PlayerState =
  { playlist :: Playlist
  , isHidden :: Boolean
  , isPlaying :: Boolean
  , hasPlayedOnce :: Boolean
  , hasHiddenOnce :: Boolean
  }

-----------
-- verbs
type ChoosePlaylist r = (choosePlaylist :: Playlist | r)
type HandleEditorInput r = (input :: EditorInput | r)
type Initialize r = (initialize :: Unit | r)
type HandleEditorOutput r = (handleEditorOutput :: EditorOutput | r)
type HandlePlayerInput r = (input :: PlayerInput | r)
type HandlePlayerOutput r = (handlePlayerOutput :: PlayerOutput | r)
type HidePlayer r = (hidePlayer :: Unit | r)
type PauseScroll r = (pauseScroll :: Unit | r)
type PlayScroll r = (playScroll :: String | r)
type PressPlay r = (pressPlay :: Unit | r)
type PressStop r = (pressStop :: Unit | r)
type ResumeScroll r = (resumeScroll :: Unit | r)
type SetAudioContext r = (setAudioContext :: AudioContext | r)
type SetCurrentPlaylist r = (setCurrentPlaylist :: PlaylistSequence | r)
type SetCursor r = (setCursor :: Int | r)
type SetIsPlaying r = (setIsPlaying :: Boolean | r)
type SetIsScrolling r = (setIsScrolling :: Boolean | r)
type SetNewWagPush r = (setNewWagPush :: AFuture -> Effect Unit | r)
type SetStopScrolling r = (setStopScrolling :: Effect Unit | r)
type SetStopWags r = (setStopWags :: Effect Unit | r)
type ShowPlayer r = (showPlayer :: Unit | r)

----
type EditorAction = Variant
  $ HandleEditorInput
  + PauseScroll
  + ResumeScroll
  + ShowPlayer
  + ()

type EditorOutput = Variant
  $ PauseScroll
  + PlayScroll
  + ShowPlayer
  + ()

type MainAction = Variant
  $ HandlePlayerOutput
  + HandleEditorOutput
  + Initialize
  + SetAudioContext
  + SetCurrentPlaylist
  + SetCursor
  + SetIsPlaying
  + SetIsScrolling
  + SetNewWagPush
  + SetStopScrolling
  + SetStopWags
  + ()

type PlayerAction = Variant
  $ ChoosePlaylist
  + HandlePlayerInput
  + HidePlayer
  + PressPlay
  + PressStop
  + ()

type PlayerOutput = Variant
  $ ChoosePlaylist
  + HidePlayer
  + PressPlay
  + PressStop
  + ()