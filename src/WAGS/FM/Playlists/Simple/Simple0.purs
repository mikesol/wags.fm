module WAGS.FM.Playlists.Simple.Simple0 where

import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 1.0 { earth: s "bd" }