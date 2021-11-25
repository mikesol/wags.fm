module Playlists.Java.Java0 where


import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s "BBPL1"
  , title: "gamelan 1"
  , sounds: Gamelan.sounds
  }