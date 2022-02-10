module Playlists.Java.Java5 where



import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s """
  <[BBPL1 BBPL2h BBPL2h BBPL3 BBPL2h BBPL2h BBPL5 BBPL2h]
  [BBPL1 BBSL2 BBPL1 BBSL2]>"""
  , wind: s """<[~ DHA*2 ~ LUNG DHA ~ TAK TAK]
  [[~ DHA*2 ~ DHA] [~ DHUNG TONG TONG]]>"""
  , sounds: Gamelan.sounds
  }