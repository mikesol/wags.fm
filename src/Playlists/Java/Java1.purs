module Playlists.Java.Java1 where

import Prelude

import Data.Lens (set, traversed)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Tidal (changeVolume, lnv, make, onTag, parse, s)
import WAGS.Lib.Tidal.Types (AFuture)

wag :: AFuture
wag = make 4.0
  { earth: s
      $ onTag "0"
          ( changeVolume
              ( add 0.8
                  <<< lfo { phase: 0.0, freq: 8.0, amp: 0.2 }
                  <<< _.clockTime
              )
          )
      $ onTag "1" (set (traversed <<< lnv) (const 0.2))
      $ parse "BBPL1;0 [BPPL7,SBSL3;1]"
  , sounds: Gamelan.sounds
  }
