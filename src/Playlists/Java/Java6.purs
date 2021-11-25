module Playlists.Java.Java6 where


import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s "<[BBPL1 BBPL2h [BBPL2h BBPL3] BBPL3 BBPL2h [BBPL2h BBPL3] BBPL5 BBPL2h] [BBPL1 BBSL2 BBPL1 BBSL2]>"
  , wind: s "<[~ DHA*2 ~ LUNG DHA ~ TAK TAK] [[~ DHA*2 ~ DHA] [~ DHUNG TONG TONG]]>"
  , title: "gamelan 1"
  , sounds: Gamelan.sounds
  }