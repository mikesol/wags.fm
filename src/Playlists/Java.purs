module Playlists.Java where

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Nouns as N
import Playlists.Java.Java0 as Java0
import Playlists.Java.Java1 as Java1
import Playlists.Java.Java2 as Java2

dummyCode = "module Foo where" :: String

playlist :: N.Playlist
playlist =
  { title: "with ♥ from java"
  , sequence: NEL.NonEmptyList
      ( { duration: Milliseconds 4000.0
        , code: dummyCode
        , wag: Java0.wag
        } :| { duration: Milliseconds 4000.0
        , code: dummyCode, wag: Java1.wag }
          :
            { duration: Milliseconds 4000.0
            , code: dummyCode
            , wag: Java2.wag
            }
          : Nil
      )
  }