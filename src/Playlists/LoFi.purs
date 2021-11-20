module Playlists.LoFi where

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Nouns as N
import Playlists.LoFi.LoFi0 as LoFi0
import Playlists.LoFi.LoFi1 as LoFi1
import Playlists.LoFi.LoFi2 as LoFi2

dummyCode = "module Foo where" :: String

playlist :: N.Playlist
playlist =
  { title: "sweet & lo-fi"
  , sequence: NEL.NonEmptyList
      ( { duration: Milliseconds 4000.0
        , code: dummyCode
        , wag: LoFi0.wag
        } :| { duration: Milliseconds 4000.0
        , code: dummyCode, wag: LoFi1.wag }
          :
            { duration: Milliseconds 4000.0
            , code: dummyCode
            , wag: LoFi2.wag
            }
          : Nil
      )
  }