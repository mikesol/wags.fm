module WAGS.FM.Emitter where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Event (Event, makeEvent)

loopEmitter'
  :: forall struct
   . Ref (Maybe TimeoutId)
  -> (struct -> Int)
  -> NonEmpty List struct
  -> (struct -> Effect Unit)
  -> List struct
  -> Effect Unit
loopEmitter' r f l@(a :| b) push Nil = loopEmitter' r f l push (a : b)
loopEmitter' r f l push (c : d) =
  setTimeout (f c)
    ( do
        push c
        loopEmitter' r f l push d
    ) >>= flip write r <<< pure

loopEmitter :: forall struct. (struct -> Int) -> NonEmpty List struct -> Event struct
loopEmitter f l@(a :| b) = makeEvent \k -> do
  ref <- new Nothing
  loopEmitter' ref f l k (a : b)
  pure (read ref >>= maybe (pure unit) clearTimeout)