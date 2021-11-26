module Playlists.Java.Java15 where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (_Just, set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Samples (normalizedSampleTime, sampleTime)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, make, onTag, parse_, s)

wag :: AFuture
wag = make 4.0
  { earth: s "BBPL1 [[~ ~ ~ BBPL6] ~] <BBPL5 BBPL2> BBPL3h"
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse_ """<[~ TONG ~ ~ DHA ~ ~ TONG]
      [[ KtSL2s TONG ~ ~] [~ DHUNG ~ ~]]>"""
  , sounds: Gamelan.sounds
  }