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

s2ms :: Number -> Milliseconds
s2ms = Milliseconds <<< mul 1000.0

playlist :: T.Playlist
playlist =
  { title: "with ♥ from java"
  , sequence: NEL.NonEmptyList
      ( { duration: s2ms 8.0
        , code: java0Code
        , wag: Java0.wag
        } :|
          { duration: s2ms 8.0
          , code: java1Code
          , wag: Java1.wag
          }
            :
              { duration: s2ms 12.0
              , code: java2Code
              , wag: Java2.wag
              }
            :
              { duration: s2ms 16.0
              , code: java3Code
              , wag: Java3.wag
              }
            :
              { duration: s2ms 32.0
              , code: java4Code
              , wag: Java4.wag
              }
            :
              { duration: s2ms 32.0
              , code: java5Code
              , wag: Java5.wag
              }
            :
              { duration: s2ms 32.0
              , code: java6Code
              , wag: Java6.wag
              }
            : Nil
      )
  }