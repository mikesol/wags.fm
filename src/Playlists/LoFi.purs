module Playlists.LoFi where

import Prelude

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Types as T
import Playlists.LoFi.LoFi0 as LoFi0
import Playlists.LoFi.LoFi1 as LoFi1
import Playlists.LoFi.LoFi2 as LoFi2
import Playlists.LoFi.LoFi3 as LoFi3
import Playlists.LoFi.LoFi4 as LoFi4
import Playlists.LoFi.LoFi5 as LoFi5

foreign import loFi0Code :: String
foreign import loFi1Code :: String
foreign import loFi2Code :: String
foreign import loFi3Code :: String
foreign import loFi4Code :: String
foreign import loFi5Code :: String

s2ms :: Number -> Milliseconds
s2ms = Milliseconds <<< mul 1000.0

playlist :: T.Playlist
playlist =
  { title: "sweet & lo-fi"
  , sequence: NEL.NonEmptyList
      ( { duration: s2ms 16.0
        , code: loFi0Code
        , wag: LoFi0.wag
        } :|
          { duration: s2ms 32.0
          , code: loFi1Code
          , wag: LoFi1.wag
          }
            :
              { duration: s2ms 32.0
              , code: loFi2Code
              , wag: LoFi2.wag
              }
            :
              { duration: s2ms 32.0
              , code: loFi3Code
              , wag: LoFi3.wag
              }
            :
              { duration: s2ms 32.0
              , code: loFi4Code
              , wag: LoFi4.wag
              }
            :
              { duration: s2ms 32.0
              , code: loFi5Code
              , wag: LoFi5.wag
              }
            : Nil
      )
  }