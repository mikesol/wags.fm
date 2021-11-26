module Playlists.LoFi.LoFi1 where

import Prelude

import Data.Lens (set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Math ((%))
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, lvt, make, onTag, parse_, s)
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Learn.Oscillator (lfo)

m2 = 4.0 * 1.0 * 60.0 / 111.0 :: Number

wag :: AFuture
wag =
  make (m2 * 2.0)
    { wind:
        map
          ( set lvt
              $ lcmap unwrap \{ clockTime } ->
                  let
                    mody = clockTime % (m2 * 2.0)
                  in
                    fx
                      $ goodbye
                      $ highpass (200.0 + mody * 100.0) hello
          ) $ s
          $ onTag "ph"
              (set (traversed <<< lnr)
              $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.2 (1.0 + t * 0.3))
          $ onTag "print"
              (set (traversed <<< lnv)
              $ lcmap unwrap \{ normalizedSampleTime: _ } -> 0.2)
          $ onTag "pk"
              (set (traversed <<< lnr)
               $ lcmap unwrap \{ normalizedSampleTime: t } -> 0.7 - t * 0.2)
          $ onTag "kt"
              (set (traversed <<< lnr)
              $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.0 (0.6 + t * 0.8))
          $ parse_ """[psr:3 [~ chin*4]]
          [~ psr:3;ph ~ psr:3;ph ~ ] ,
          [~ ~ ~ <psr:1;print kurt:0;print> ] kurt:5;kt ,
          ~ ~ pluck:1;pk ~ ~ ~ ~ ~ """
    , earth: s "~ insect:2 ~ insect ~ insect:1"
    }