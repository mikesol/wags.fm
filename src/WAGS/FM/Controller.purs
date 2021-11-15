module WAGS.FM.Controller where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (for_)
import Data.Int (round)
import Data.List (List)
import Data.List as List
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Ref (new, read, write)
import FRP.Event (subscribe)
import WAGS.FM.Emitter (loopEmitter)
import WAGS.FM.Types (Playlist)
import WAGS.Lib.Tidal (AFuture)

type Snippet = Int
type SetSnippet = Int -> Effect Unit
----------
type IsScrolling = Boolean
type SetIsScrolling = Boolean -> Effect Unit
----------
type IsPlaying = Boolean
type SetIsPlaying = Boolean -> Effect Unit
----------
type StopScrolling = Effect Unit
type SetStopScrolling = Effect Unit -> Effect Unit
----------
type StopWags = Effect Unit
type SetStopWags = Effect Unit -> Effect Unit
----------
type NewWagPush = { push :: AFuture -> Effect Unit }
type SetNewWagPush = { push :: AFuture -> Effect Unit } -> Effect Unit

type PauseScrollSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setIsScrolling :: SetIsScrolling
  }
  -> Effect Unit

----

type StopWagsSig =
  { stopScrolling :: StopScrolling
  , setStopScrolling :: SetStopScrolling
  , setIsScrolling :: SetIsScrolling
  , setIsPlaying :: SetIsPlaying
  , stopWags :: StopWags
  , setStopWags :: SetStopWags
  }
  -> Effect Unit

---

type PlayScrollSig =
  { snippet :: Snippet
  , setStopScrolling :: SetStopScrolling
  , setSnippet :: SetSnippet
  , isScrolling :: IsScrolling
  , setIsScrolling :: SetIsScrolling
  , newWagPush :: NewWagPush
  , playlist :: Playlist
  }
  -> Effect Unit

---

type PlayWagsSig =
  { snippet :: Snippet
  , setStopScrolling :: SetStopScrolling
  , setSnippet :: SetSnippet
  , setIsScrolling :: SetIsScrolling
  , isPlaying :: IsPlaying
  , setIsPlaying :: SetIsPlaying
  , setStopWags :: SetStopWags
  , playlist :: Playlist
  }
  -> Effect Unit

stopWags :: StopWagsSig
stopWags
  { setIsScrolling
  , setIsPlaying
  , stopScrolling
  , setStopScrolling
  , stopWags: sw
  , setStopWags
  } =
  setIsScrolling false
    *> setIsPlaying false
    *> stopScrolling
    *> setStopScrolling (pure unit)
    *> sw
    *> setStopWags (pure unit)

pauseScroll :: PauseScrollSig
pauseScroll { setIsScrolling, stopScrolling, setStopScrolling } =
  setIsScrolling false *> stopScrolling *> setStopScrolling (pure unit)

al :: NonEmpty Array ~> NonEmpty List
al (a :| b) = a :| (List.fromFoldable b)

playScroll :: PlayScrollSig
playScroll
  { snippet
  , setSnippet
  , isScrolling
  , setIsScrolling
  , setStopScrolling
  , newWagPush
  , playlist
  } =
  for_ (map (al <<< NEA.toNonEmpty) (NEA.fromArray playlist)) \nea ->
    when (not isScrolling) do
      pg <- new snippet
      stopScrolling <- subscribe (loopEmitter (_.duration >>> mul 1000.0 >>> round) $ nea) \{ wag } -> do
        pg' <- read pg
        let np = pg' + 1
        setSnippet np
        write np pg
        newWagPush.push wag
      setIsScrolling true
      setStopScrolling stopScrolling
