module Playlists.Blank.Blank where

import Prelude

import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag =
  make 1.0
    { earth: s "hh"
    , title: "i m a k e n o i s e"
    }
