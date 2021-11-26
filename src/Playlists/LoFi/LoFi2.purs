module Playlists.LoFi.LoFi2 where

import Prelude

import Data.Lens (_Just, set, traversed)
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
    { earth: s
        $ set (traversed <<< _Just <<< lnr)
            ( lcmap unwrap \{ normalizedLittleCycleTime: t } ->
                1.0 + t * 0.1
            )
        $ parse_
            """tink:1 tink:2 tink:3 tink:0
        tink:4 tink:2 tink:3 tink:1
        tink:2 tink:0 tink:3 """
    , wind:
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
              (set (traversed <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.2 (1.0 + t * 0.3))
          $ onTag "print"
              (set (traversed <<< lnv) $ lcmap unwrap \{ normalizedSampleTime: _ } -> 0.2)
          $ onTag "pk"
              (set (traversed <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> 0.7 - t * 0.2)
          $ onTag "kt"
              (set (traversed <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.0 (0.6 + t * 0.8))
          $ parse_
              """[psr:3 [~ chin*4]]
          [~ psr:3;ph psr:3;ph ~ ] ,
          [~ ~ ~ <psr:1;print kurt:0;print> ] kurt:5;kt ,
          ~ ~ pluck:1;pk ~ ~ ~ ~ ~ """
    , fire:
        map
          ( set lvt
              ( lcmap unwrap \{ clockTime } -> fx
                  ( goodbye $ pan
                      ( lfo
                          { phase: 0.0
                          , amp: 1.0
                          , freq: 0.2
                          }
                          clockTime + 0.0
                      )
                      { myhp:
                          highpass
                            ( lfo
                                { phase: 0.0
                                , amp: 2000.0
                                , freq: 0.4
                                }
                                clockTime + 2000.0
                            )
                            hello
                      }
                  )
              )
          ) $ s "~ insect:2 ~ insect ~ insect:1 ~ speechless:2 ~"
    }