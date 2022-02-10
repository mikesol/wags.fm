module Playlists.Java.Java12 where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (_Just, set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Samples (normalizedSampleTime, sampleTime)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, make, onTag, parse, s)

wag :: AFuture
wag = make 4.0
  { earth: s
  $ parse """BBPL1 ~ ~ BBPL3
  ~ BBPL2h BBPL5 ~
  BBPL2h BBPL4 ~ BBPL6
  BBPL3h ~ BBPL4 ~
  BBPL1 ~ ~ ~
  BBPL2h ~ BBPL5 ~
  BBPL2h BBPL4 ~ BBPL6
  ~ ~ BBPL4 ~"""
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse """<[GBPL1;g1 TAK SBPL1 LUNG DHA ~ ~ TAK]
      [[GBPL3 DHA*2 SBPL4;g1 ~] [~ DHUNG ~ TONG]]>"""
  , sounds: Gamelan.sounds
  }