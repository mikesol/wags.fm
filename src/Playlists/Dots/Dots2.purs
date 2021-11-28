module Playlists.Dots.Dots2 where

import Prelude

import Data.Homogeneous.Record (homogeneous, fromHomogeneous)
import Data.Int (toNumber)
import Data.Lens (set, traversed)
import Data.Maybe (maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Math (pow)
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Cycle (cycleFromSample_, cycleLength, r)
import WAGS.Lib.Tidal.Tidal (i, lnr, make, s, x)
import WAGS.Lib.Tidal.Types (Sample(..))

wag :: AFuture
wag =
  make 1.0
    { earth: s ""
    , title: "i m a k e n o i s e"
    }

{-


import Prelude

import Data.Homogeneous.Record (homogeneous, fromHomogeneous)
import Data.Int (toNumber)
import Data.Lens (set, traversed)
import Data.Maybe (maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Math (pow)
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Cycle (cycleFromSample_, cycleLength, r)
import WAGS.Lib.Tidal.Tidal (i, lnr, make, s, x)
import WAGS.Lib.Tidal.Types (Sample(..))

dt = 0.11875 :: Number

seq0 = n.g4 :|
  [ r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.e4, r, n.e4, n.e4, n.e4, n.a4, n.a4, n.g4, r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.a4, r, n.fs4, n.d4, r, n.d4, n.e4, n.fs4, n.g4, n.a4, r, n.a4, n.a4 ]
seq1 = n.g4 :|
  [ r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.e4, r, n.e4, n.e4, n.e4, n.a4, n.a4, n.g4, r, n.g4, n.fs4, r, n.fs4, r, n.fs4, n.e4, r, n.e4, r, n.e4, n.a4, n.fs4, n.d4, r, n.d4, n.e4, r, n.fs4, n.g4, n.a4, r, n.a4, n.a4 ]
seq2 = n.g4 :|
  [ r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.e4, r, n.e4, n.e4, r, n.e4, r, n.e4, r, n.e4, n.e4, r, n.e4, r, n.e4, r, n.e4, n.e4, n.e4, n.a4, n.a4, n.g4, r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.e4, n.d4, n.d4, n.e4, n.fs4, n.g4, n.a4, r, n.a4, n.a4 ]
seq3 = n.g4 :|
  [ r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.e4, r, n.e4, n.e4, n.e4, n.a4, n.a4, n.g4, r, n.g4, n.fs4, r, n.fs4, r, n.e4, r, n.a4, r, n.fs4, n.d4, r, n.d4, n.e4, n.fs4, n.g4, n.a4, r, n.d4, n.e4, n.fs4, n.g4, n.a4, r, n.d4, n.e4, r, n.d4, n.e4, n.g4, n.a4 ]

seq = i <^> (seq0 <.> seq1 <.> seq2 <.> seq3)

dscnt = seq >>= maybe r (const n.a4)

wag :: AFuture
wag =
  make (mul dt $ toNumber $ cycleLength seq)
    { earth: s $ x seq [ dscnt ]
    , title: "i m a k e n o i s e"
    }

data UDN = U | D | N

nappend
  :: forall f a
   . Semigroup (f a)
  => Applicative f
  => NonEmpty f a
  -> NonEmpty f a
  -> NonEmpty f a
nappend (h0 :| t0) (h1 :| t1) = h0 :| (t0 <> pure h1 <> t1)

infixr 5 nappend as <.>

napply f (h :| t) = f h t

infixr 5 napply as <^>


bend :: UDN -> Number
bend N = 1.0
bend U = 1.0 * (2.0 `pow` (1.0 / 12.0))
bend D = 1.0 * (2.0 `pow` (-1.0 / 12.0))

n = fromHomogeneous
  $ map
      ( ( set (traversed <<< traversed <<< lnr)
            <<< const
            <<< bend
            <<< fst
        ) <*> (cycleFromSample_ <<< Sample <<< snd)
      )
  $ homogeneous
      { cs4: N /\ "notes:0"
      , d4: U /\ "notes:0"
      , ds4: N /\ "notes:1"
      , e4: U /\ "notes:1"
      , f4: D /\ "notes:7"
      , fs4: N /\ "notes:7"
      , g4: U /\ "notes:7"
      , gs4: N /\ "notes:8"
      , a4: U /\ "notes:8"
      , as4: N /\ "notes:9"
      , b4: U /\ "notes:9"
      , c5: D /\ "notes:10"
      , cs5: N /\ "notes:10"
      , d5: U /\ "notes:10"
      , ds5: N /\ "notes:11"
      , e5: U /\ "notes:11"
      , f5: D /\ "notes:12"
      , fs5: N /\ "notes:12"
      , g5: U /\ "notes:12"
      , gs5: N /\ "notes:13"
      , a5: U /\ "notes:13"
      , as5: N /\ "notes:14"
      , b5: U /\ "notes:14"
      , c6: D /\ "notes:2"
      , cs6: N /\ "notes:2"
      , d6: U /\ "notes:2"
      , ds6: N /\ "notes:3"
      , e6: U /\ "notes:3"
      , f6: D /\ "notes:4"
      , fs6: N /\ "notes:4"
      , g6: U /\ "notes:4"
      , gs6: N /\ "notes:5"
      , a6: U /\ "notes:5"
      , as6: N /\ "notes:6"
      , b6: U /\ "notes:6"
      }

-}