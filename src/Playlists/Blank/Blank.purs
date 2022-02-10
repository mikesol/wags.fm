module Playlists.Blank.Blank where

import Prelude

import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag =
  make 1.0
    { earth: s "hh"
    }
