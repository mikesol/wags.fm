module Playlists.Java.Java14 where

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
  $ parse """BBPL1 [~ BBPL2h ~ ~]
  [~ ~ ~ BBPL6] [~ BBPL4]
  <BBPL5 BBPL2> BBPL2h
  [BBPL3h ~ ~ BBPL6] ~"""
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse """<[~ TONG ~ ~ DHA ~ ~ TONG]
      [[ KtSL2s DHA*2 SBPL4;g1 ~] [~ DHUNG ~ TONG]]>"""
  , sounds: Gamelan.sounds
  }