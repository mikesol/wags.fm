module Playlists.LoFi.LoFi4 where

import Prelude

import Data.Lens (_Just, set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Math ((%))
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, lvt, make, onTag, parse, s)

m2 = 4.0 * 1.0 * 60.0 / 111.0 :: Number

wag :: AFuture
wag =
  make (m2 * 2.0)
    { earth: s
        $ onTag "nn"
              (set (traversed <<< lnv) $ (const 0.25))
        $ parse
            """tink:1 ~ ~ tink:0
        tink:4 ~ ~ tink:1
        tink:2 ~ ~ , ~ newnotes:2;nn newnotes:3;nn newnotes:4;nn
        newnotes:2;nn newnotes:3;nn newnotes:4;nn newnotes:6;nn
        ~ newnotes:7;nn newnotes:1;nn newnotes:8;nn """
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
          $ parse
              """[psr:3 [~ chin*4]]
          [~ psr:3;ph psr:3;ph ~ ] ,
          [~ ~ ~ <psr:10;print kurt:8;print> ] kurt:10;kt ,
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
          ) $ s """~ insect:2 ~ insect stomp:4*3 insect:1 ~ speechless:2 ~ , speechless:5 speechless:3 speechless:4 speechless:1 speechless:2 """
    }