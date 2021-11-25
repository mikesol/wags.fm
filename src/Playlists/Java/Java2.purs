module Playlists.Java.Java2 where

import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s "BBPL1 BPPL7"
  , wind: s "~ DHA*2 ~ DHA ~ ~ TAK TAK"
  , title: "gamelan 1"
  , sounds: Gamelan.sounds
  }