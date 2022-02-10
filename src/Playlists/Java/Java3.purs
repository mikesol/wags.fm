module Playlists.Java.Java3 where


import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (make, s)

wag :: AFuture
wag = make 4.0
  { earth: s "<[BBPL1 BBPL6 ~ BPPL7] [BBPL1 BBSL2]>"
  , wind: s """<[~ DHA*2 ~ DHA ~ ~ TAK TAK]
   [[~ DHA*2 ~ DHA] [~ TONG TONG TONG]]>"""
  , sounds: Gamelan.sounds
  }