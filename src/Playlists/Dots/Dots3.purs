module Playlists.Dots.Dots3 where

import Prelude

import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Tidal (lnr, lvt, make, parse_, s)
import Data.Lens (set, traversed, _Just)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Samples (littleCycleTime)

dt = 0.11875 :: Number

wag :: AFuture
wag =
  make (dt * 9.0)
    { earth: s $
        """notes:0 ~
      <notes:2 notes:13 notes:14> ~
      <notes:4 notes:10> ~
      <notes:6 notes:14> ~
      <notes:8 notes:9>"""
    , fire:
        map
          ( set lvt
              $ lcmap unwrap \{ clockTime } -> fx
                  $ goodbye
                  $ pan
                      (lfo { phase: 0.0, amp: 1.0, freq: 0.1 } clockTime)
                      { myhp: highpass
                          ( lfo { phase: 0.0, amp: 1500.0, freq: 0.4 }
                              clockTime + 1510.0
                          )
                          hello
                      }
          ) $ s $
          """~ <notes:1 notes:8>
      ~ notes:3
      ~ <notes:5 notes:9 notes:2>
      ~ notes:7 ~"""
    , wind: s
        $ set (traversed <<< _Just <<< lnr)
            (lcmap littleCycleTime (add 0.5 <<< mul 0.5))
        $ parse_
        $ "chin:0 ~ chin:1 ~ ~ chin:2 ~ chin:3"
    }