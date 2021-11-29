module Playlists.Blank where

import Prelude

import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Playlists.Blank.Blank as Blank
import Types as T

foreign import blankCode :: String

s2ms :: Number -> Milliseconds
s2ms = Milliseconds <<< mul 1000.0

playlist :: T.Playlist
playlist =
  { title: "blank slate"
  , sequence: NEL.NonEmptyList
      ( { duration: s2ms 1.0
        , code: blankCode
        , wag: Blank.wag
        } :| Nil
      )
  }