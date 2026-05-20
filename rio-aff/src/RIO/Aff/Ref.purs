-- | A plain atomic mutable reference.
-- |
-- | `Ref a` is the simplest shared-mutable-state primitive: a cell
-- | holding a single value of type `a`. Every operation is atomic
-- | by virtue of running synchronously in `Effect`; in the
-- | single-threaded JavaScript runtime there is no point at which
-- | a read/modify/write can be interleaved with another fiber.
-- |
-- | This is the `RIO`-flavored counterpart of `Effect.Ref`. The
-- | semantics are identical; the value is the API discoverability
-- | (`RIO.Aff.Ref.new`, `RIO.Aff.Ref.modify`, ...) and the consistent row
-- | of `RIO r e`. If you only need state inside a single
-- | transaction, prefer `RIO.Aff.STM.TRef`. If you need scoped
-- | overrides on the dynamic extent of a block, prefer
-- | `RIO.Aff.Local`. Use `Ref` when you want plain shared state
-- | across fibers with no scoping or transactionality.
-- |
-- | ## Atomicity caveat: effectful updates
-- |
-- | `modify` and `update` take a *pure* function and are atomic.
-- | If you need an effectful update that must observe the cell's
-- | latest value without races, use `RIO.Aff.STM.TRef` inside an
-- | `atomically` block instead. There is no `modifyM`-style
-- | combinator here on purpose: any `RIO`-typed update body could
-- | yield to other fibers between read and write, so making it
-- | look atomic would be misleading.
module RIO.Aff.Ref
  ( Ref
  , new
  , newEffect
  , read
  , write
  , modify
  , modify_
  , update
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref) as ERef
import Effect.Ref as ERef

import RIO.Aff.Internal (RIO(..), mkEffectRIO)

-- | A mutable cell of type `a`. The constructor is hidden; use
-- | `new` or `newEffect` to create one.
newtype Ref a = Ref (ERef.Ref a)

-- | Create a fresh `Ref` initialised to `value`.
new :: forall r e a. a -> RIO r e (Ref a)
new value = mkEffectRIO \_ -> Ref <$> ERef.new value

-- | `Effect`-typed variant for callers that allocate state at
-- | the top of `main` before entering `RIO`.
newEffect :: forall a. a -> Effect (Ref a)
newEffect value = Ref <$> ERef.new value

-- | Read the current value.
read :: forall r e a. Ref a -> RIO r e a
read (Ref ref) = mkEffectRIO \_ -> ERef.read ref

-- | Overwrite the value, discarding the previous one.
write :: forall r e a. Ref a -> a -> RIO r e Unit
write (Ref ref) value = mkEffectRIO \_ -> ERef.write value ref

-- | Apply a pure function to the current value and store the
-- | result. Returns the new value.
modify :: forall r e a. Ref a -> (a -> a) -> RIO r e a
modify (Ref ref) f = mkEffectRIO \_ -> ERef.modify f ref

-- | Apply a pure function and discard the result.
modify_ :: forall r e a. Ref a -> (a -> a) -> RIO r e Unit
modify_ (Ref ref) f = mkEffectRIO \_ -> ERef.modify_ f ref

-- | Alias for `modify_` that reads naturally at call sites.
update :: forall r e a. Ref a -> (a -> a) -> RIO r e Unit
update = modify_
