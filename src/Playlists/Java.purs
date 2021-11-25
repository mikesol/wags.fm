module Playlists.Java where

import Prelude

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Playlists.Java.Java0 as Java0
import Playlists.Java.Java1 as Java1
import Playlists.Java.Java2 as Java2
import Playlists.Java.Java3 as Java3
import Playlists.Java.Java4 as Java4
import Playlists.Java.Java5 as Java5
import Playlists.Java.Java6 as Java6
import Types as T

foreign import java0Code :: String
foreign import java1Code :: String
foreign import java2Code :: String
foreign import java3Code :: String
foreign import java4Code :: String
foreign import java5Code :: String
foreign import java6Code :: String

duration = Milliseconds (30.0 * 1_000.0) :: Milliseconds

playlist :: T.Playlist
playlist =
  { title: "with ♥ from java"
  , sequence: NEL.NonEmptyList
      ( { duration
        , code: java0Code
        , wag: Java0.wag
        } :|
          { duration
          , code: java1Code
          , wag: Java1.wag
          }
            :
              { duration
              , code: java2Code
              , wag: Java2.wag
              }
            :
              { duration
              , code: java3Code
              , wag: Java3.wag
              }
            :
              { duration
              , code: java5Code
              , wag: Java5.wag
              }
            :
              { duration
              , code: java6Code
              , wag: Java6.wag
              }
            : Nil
      )
  }