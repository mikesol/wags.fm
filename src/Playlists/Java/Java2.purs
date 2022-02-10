module Playlists.Java.Java2 where

import Prelude

import Data.Lens (_Just, set, traversed)
import Data.Profunctor (lcmap)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Samples (clockTime)
import WAGS.Lib.Tidal.Tidal (make, parse, onTag, lnv, s)

wag :: AFuture
wag = make 4.0
  { earth: s
      $ onTag "0"
          ( set (traversed <<< lnv)
              $ lcmap clockTime
                  $ add 0.8 <<<
                      lfo { phase: 0.0, freq: 8.0, amp: 0.2 }
          )
      $ parse "BBPL1;0 BPPL7"
  , wind: s "~ DHA*2 ~ DHA ~ ~ TAK TAK"
  , sounds: Gamelan.sounds
  }