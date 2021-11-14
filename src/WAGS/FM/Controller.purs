module WAGS.FM.Controller where

import Prelude

import Effect (Effect)
import FRP.Event (subscribe)
import WAGS.FM.Emitter (loopEmitter)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Effect.Ref (new, read, write)

type Snippet = Int
type SetSnippet = Int -> Effect Unit
type IsPlaying = Boolean
type SetIsPlaying = Boolean -> Effect Unit
type StopPlaying = Effect Unit
type SetStopPlaying = Effect Unit -> Effect Unit
type PauseSig =
  { stopPlaying :: StopPlaying
  , setStopPlaying :: SetStopPlaying
    , setIsPlaying :: SetIsPlaying

  }
  -> Effect Unit

type PlaySig =
  { snippet :: Snippet
  , setStopPlaying :: SetStopPlaying
  , setSnippet :: SetSnippet
  , isPlaying :: IsPlaying
  , setIsPlaying :: SetIsPlaying
  }
  -> Effect Unit

pause :: PauseSig
pause { setIsPlaying, stopPlaying, setStopPlaying } =
  setIsPlaying false *> stopPlaying *> setStopPlaying (pure unit)

play :: PlaySig
play { snippet, setSnippet, isPlaying, setIsPlaying, setStopPlaying } =
  when (not isPlaying) do
    pg <- new snippet
    stopPlaying <- subscribe (loopEmitter $ 2000 :| (4000 : 6000 : Nil)) $ const do
      pg' <- read pg
      let np = pg' + 1
      setSnippet np
      write np pg
    setIsPlaying true
    setStopPlaying stopPlaying
