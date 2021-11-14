module WAGS.FM.Emitter where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Event (Event, makeEvent)

loopEmitter' :: Ref (Maybe TimeoutId) -> NonEmpty List Int -> (Unit -> Effect Unit) -> List Int -> Effect Unit
loopEmitter' r l@(a :| b) push Nil = loopEmitter' r l push (a : b)
loopEmitter' r l push (c : d) =
  setTimeout c
    ( do
        push unit
        loopEmitter' r l push d
    ) >>= flip write r <<< pure

loopEmitter :: NonEmpty List Int -> Event Unit
loopEmitter l@(a :| b) = makeEvent \k -> do
  ref <- new Nothing
  loopEmitter' ref l k (a : b)
  pure (read ref >>= maybe (pure unit) clearTimeout)