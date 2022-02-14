module Playlists.Java.Java15 where

import Prelude

import Data.Lens (set, traversed)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (lnv, make, onTag, parse, s)

wag :: AFuture
wag = make 4.0
  { earth: s "BBPL1 [[~ ~ ~ BBPL6] ~] <BBPL5 BBPL2> BBPL3h"
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse """<[~ TONG ~ ~ DHA ~ ~ TONG]
      [[ KtSL2s TONG ~ ~] [~ DHUNG ~ ~]]>"""
  , sounds: Gamelan.sounds
  }