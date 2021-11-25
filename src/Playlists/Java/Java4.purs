module Playlists.Java.Java4 where


import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s "<[BBPL1 BBPL6 BPPL7 ~ BBPL5 BBPL7h] [BBPL1 BBSL2 BBPL1 BBSL2]>"
  , wind: s "<[~ DHA*2 ~ DHA ~ ~ TAK TAK] [[~ DHA*2 ~ DHA] [~ TONG TONG TONG]]>"
  , title: "gamelan 1"
  , sounds: Gamelan.sounds
  }