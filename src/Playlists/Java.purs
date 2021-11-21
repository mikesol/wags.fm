module Playlists.Java where

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Types as T
import Playlists.Java.Java0 as Java0
import Playlists.Java.Java1 as Java1
import Playlists.Java.Java2 as Java2

foreign import java0Code :: String
foreign import java1Code :: String
foreign import java2Code :: String

playlist :: T.Playlist
playlist =
  { title: "with ♥ from java"
  , sequence: NEL.NonEmptyList
      ( { duration: Milliseconds 4000.0
        , code: java0Code
        , wag: Java0.wag
        } :|
          { duration: Milliseconds 4000.0
          , code: java1Code
          , wag: Java1.wag
          }
            :
              { duration: Milliseconds 4000.0
              , code: java2Code
              , wag: Java2.wag
              }
            : Nil
      )
  }