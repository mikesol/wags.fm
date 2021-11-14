module WAGS.FM.Controller where

import Prelude

import Effect (Effect)
import FRP.Event (subscribe)
import WAGS.FM.Emitter (loopEmitter)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Effect.Ref (new, read, write)

type Page = Int
type SetPage = Int -> Effect Unit
type IsPlaying = Boolean
type SetIsPlaying = Boolean -> Effect Unit
type StopPlaying = Effect Unit
type SetStopPlaying = Effect Unit -> Effect Unit
type PauseSig =
  { stopPlaying :: StopPlaying
  , setStopPlaying :: SetStopPlaying
  }
  -> Effect Unit

type PlaySig =
  { initialPage :: Page
  , setStopPlaying :: SetStopPlaying
  , setPage :: SetPage
  , isPlaying :: IsPlaying
  , setIsPlaying :: SetIsPlaying
  }
  -> Effect Unit

pause :: PauseSig
pause { stopPlaying, setStopPlaying } =
  stopPlaying *> setStopPlaying (pure unit)

play :: PlaySig
play { initialPage, setPage, isPlaying, setIsPlaying, setStopPlaying } =
  if isPlaying then pure unit
  else do
    pg <- new initialPage
    stopPlaying <- subscribe (loopEmitter $ 1000 :| (2000 : 4000 : Nil)) $ const do
      pg' <- read pg
      let np = pg' + 1
      setPage np
      write np pg
    setIsPlaying true
    setStopPlaying stopPlaying

controller :: { pause :: PauseSig, play :: PlaySig }
controller =
  { pause
  , play
  }