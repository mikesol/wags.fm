module Playlists.Java.Java0 where

import Prelude

import Data.Newtype (unwrap)
import Data.Lens (_Just, set, traversed)
import Data.Profunctor (lcmap)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (make, parse_, onTag, lnv, s)

wag :: AFuture
wag = make 4.0
  { earth: s
      $ onTag "0"
          ( set (traversed <<< lnv)
              $ lcmap unwrap
                  \{ normalizedSampleTime, clockTime } -> 0.8 +
                    lfo
                      { phase: 0.0
                      , freq: 12.0 - 8.0 * normalizedSampleTime
                      , amp: 0.2
                      }
                      clockTime

          )
      $ parse_ "BBPL1;0 BPPL7"
  , sounds: Gamelan.sounds
  }