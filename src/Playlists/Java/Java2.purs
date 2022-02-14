module Playlists.Java.Java2 where

import Prelude

import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Tidal (changeVolume, make, onTag, parse, s)
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
      $ parse "BBPL1;0 BPPL7"
  , wind: s "~ DHA*2 ~ DHA ~ ~ TAK TAK"
  , sounds: Gamelan.sounds
  }