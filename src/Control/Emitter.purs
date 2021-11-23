module Control.Emitter where

import Prelude

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
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
loopEmitter' r f l push (c : d) = do
  -- push `c` first, as this starts the section
  push c
  -- we set the timeout after
  -- this is in a do-bloc to delay the bind, otherwise there'd be a stack explosion
  -- in the calling of loopEmitter'
  setTimeout (f c) (loopEmitter' r f l push d) >>= flip write r <<< pure

shiftNel :: Int -> NonEmpty List ~> NonEmpty List
shiftNel v = unwrap <<< (go <$> (mod v <<< NEL.length) <*> identity) <<< NonEmptyList
  where
  go :: Int -> NonEmptyList ~> NonEmptyList
  go 0 x = x
  go _ nel@(NonEmptyList (_ :| Nil)) = nel
  go n (NonEmptyList (a :| (b : c))) = go (n - 1) (NonEmptyList (b :| (c <> pure a)))

loopEmitter :: forall struct. (struct -> Int) -> Int -> NonEmpty List struct -> Event struct
loopEmitter f n = lcmap (shiftNel n) \l@(a :| b) -> makeEvent \k -> do
  ref <- new Nothing
  loopEmitter' ref f l k (a : b)
  pure (read ref >>= maybe (pure unit) clearTimeout)