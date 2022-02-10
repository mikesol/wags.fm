module Playlists.Java.Java8 where

import Prelude

import Data.Lens (_Just, set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Samples (normalizedSampleTime, sampleTime)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, make, onTag, parse_, s)

wag :: AFuture
wag = make 4.0
  { earth: s """BBPL1 BBPL2h BBPL2h BBPL3
  BBPL2h BBPL2h BBPL5 BBPL2h
  BBPL2h BBPL4 BBPL6 BBPL6
  BBPL3h BBPL2h BBPL4 BBPL2h"""
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse_ """<[GBPL1;g1 DHA*2 ~ LUNG DHA ~ TAK TAK]
      [[GBPL3 DHA*2 ~ DHA] [~ DHUNG TONG TONG]]>"""
  , sounds: Gamelan.sounds
  }