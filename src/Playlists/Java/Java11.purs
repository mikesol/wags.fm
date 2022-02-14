module Playlists.Java.Java11 where

import Prelude

import Data.Lens (set, traversed)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (lnv, make, onTag, parse, s)

wag :: AFuture
wag = make 4.0
  { earth: s
      $ parse """BBPL1 ~ BBPL2h BBPL3
      ~ BBPL2h BBPL5 ~
      BBPL2h BBPL4 ~ BBPL6
      BBPL3h BBPL2h BBPL4 ~
      BBPL1 BBPL2h BBPL2h ~
      BBPL2h BBPL2h BBPL5 ~
      BBPL2h BBPL4 ~ BBPL6
      BBPL3h ~ BBPL4 BBPL2h"""
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse """<[GBPL1;g1 TAK ~ LUNG DHA ~ ~ TAK]
      [[GBPL3 DHA*2 ~ DHA] [~ DHUNG ~ TONG]]>"""
  , sounds: Gamelan.sounds
  }