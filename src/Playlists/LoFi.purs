module Playlists.LoFi where

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Time.Duration (Milliseconds(..))
import Types as T
import Playlists.LoFi.LoFi0 as LoFi0
import Playlists.LoFi.LoFi1 as LoFi1
import Playlists.LoFi.LoFi2 as LoFi2

foreign import loFi0Code :: String
foreign import loFi1Code :: String
foreign import loFi2Code :: String

playlist :: T.Playlist
playlist =
  { title: "sweet & lo-fi"
  , sequence: NEL.NonEmptyList
      ( { duration: Milliseconds 4000.0
        , code: loFi0Code
        , wag: LoFi0.wag
        } :|
          { duration: Milliseconds 4000.0
          , code: loFi1Code
          , wag: LoFi1.wag
          }
            :
              { duration: Milliseconds 4000.0
              , code: loFi2Code
              , wag: LoFi2.wag
              }
            : Nil
      )
  }