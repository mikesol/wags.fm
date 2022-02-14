module Playlists.Java.Java10 where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (set, traversed)
import WAGS.Lib.Sounds.Gamelan as Gamelan
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, make, onTag, parse, s)

seq :: String
seq = """BBPL1 BBPL2h BBPL2h BBPL3
      BBPL2h BBPL2h BBPL5 BBPL2h
      BBPL2h BBPL4 BBPL6 BBPL6
      BBPL3h BBPL2h BBPL4 BBPL2h
      BBPL1 BBPL2h BBPL2h BBPL3
      BBPL2h BBPL2h BBPL5 BBPL2h
      BBPL2h BBPL4 BBPL6 BBPL6
      BBPL3h BBPL2h BBPL4 BBPL2h"""

wag :: AFuture
wag = make 4.0
  { earth: s
      $ map ((set (traversed <<< lnv) (const (0.6))))
      $ mapWithIndex (\i -> (set (traversed <<< lnr) (const (1.0 + toNumber i * 0.01))))
      $ parse seq
  , wind: s
      $ onTag "g1" (set (traversed <<< lnv) (const 0.5))
      $ parse "<[GBPL1;g1 TAK ~ LUNG DHA ~ ~ TAK] [[[GBPL3 , KtSL2s] DHA*2 ~ DHA] [~ DHUNG ~ TONG]]>"
  , fire: s
      $ parse seq
  , sounds: Gamelan.sounds
  }