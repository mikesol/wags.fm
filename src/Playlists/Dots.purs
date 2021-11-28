module Playlists.Dots where

import Prelude

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Playlists.Dots.Dots0 as Dots0
import Playlists.Dots.Dots1 as Dots1
import Playlists.Dots.Dots2 as Dots2
import Playlists.Dots.Dots3 as Dots3
import Types as T

foreign import dots0Code :: String
foreign import dots1Code :: String
foreign import dots2Code :: String
foreign import dots3Code :: String

s2ms :: Number -> Milliseconds
s2ms = Milliseconds <<< mul 1000.0

playlist :: T.Playlist
playlist =
  { title: "••d••o••t••s••"
  , sequence: NEL.NonEmptyList
      ( { duration: s2ms 8.0
        , code: dots0Code
        , wag: Dots0.wag
        } :|
          { duration: s2ms 8.0
          , code: dots1Code
          , wag: Dots1.wag
          }
            :
              { duration: s2ms 20.0
              , code: dots2Code
              , wag: Dots2.wag
              }
            :
              { duration: s2ms 40.0
              , code: dots3Code
              , wag: Dots3.wag
              }
            : Nil
      )
  }