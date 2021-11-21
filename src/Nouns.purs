module Nouns where

import Data.List.Types (NonEmptyList)
import Data.Time.Duration (Milliseconds)
import WAGS.Lib.Tidal (AFuture)

type Cursor = Int

type Playlist =
  { title :: String
  , sequence ::
      NonEmptyList
        { duration :: Milliseconds
        , code :: String
        , wag :: AFuture
        }
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
