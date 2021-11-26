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
import Playlists.Java.Java7 as Java7
import Playlists.Java.Java8 as Java8
import Playlists.Java.Java9 as Java9
import Playlists.Java.Java10 as Java10
import Playlists.Java.Java11 as Java11
import Playlists.Java.Java12 as Java12
import Playlists.Java.Java13 as Java13
import Playlists.Java.Java14 as Java14
import Playlists.Java.Java15 as Java15
import Types as T

foreign import java0Code :: String
foreign import java1Code :: String
foreign import java2Code :: String
foreign import java3Code :: String
foreign import java4Code :: String
foreign import java5Code :: String
foreign import java6Code :: String
foreign import java7Code :: String
foreign import java8Code :: String
foreign import java9Code :: String
foreign import java10Code :: String
foreign import java11Code :: String
foreign import java12Code :: String
foreign import java13Code :: String
foreign import java14Code :: String
foreign import java15Code :: String

s2ms :: Number -> Milliseconds
s2ms = Milliseconds <<< mul 1000.0

playlist :: T.Playlist
playlist =
  { title: "with ♥ from java"
  , sequence: NEL.NonEmptyList
      ( { duration: s2ms 16.0
        , code: java0Code
        , wag: Java0.wag
        } :|
          { duration: s2ms 12.0
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
            ----
            :
              { duration: s2ms 32.0
              , code: java7Code
              , wag: Java7.wag
              }
            :
              { duration: s2ms 32.0
              , code: java8Code
              , wag: Java8.wag
              }
            :
              { duration: s2ms 32.0
              , code: java9Code
              , wag: Java9.wag
              }
            :
              { duration: s2ms 32.0
              , code: java10Code
              , wag: Java10.wag
              }
            :
              { duration: s2ms 32.0
              , code: java11Code
              , wag: Java11.wag
              }
            :
              { duration: s2ms 32.0
              , code: java12Code
              , wag: Java12.wag
              }
            :
              { duration: s2ms 32.0
              , code: java13Code
              , wag: Java13.wag
              }
            :
              { duration: s2ms 32.0
              , code: java14Code
              , wag: Java14.wag
              }
            :
              { duration: s2ms 32.0
              , code: java15Code
              , wag: Java15.wag
              }
            : Nil
      )
  }