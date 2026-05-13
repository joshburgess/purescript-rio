-- | Phase 4 review stress test.
-- |
-- | Two scenarios:
-- |
-- |   * `runDeepNested`: open `target` nested scopes, each registering
-- |     a finalizer. Optionally terminate at a chosen depth via a
-- |     typed failure or a defect. Returns the recorded event log.
-- |
-- |   * `runDeepNestedSleep`: same shape, but the innermost body
-- |     sleeps so the surrounding fiber can be killed mid-flight.
-- |
-- | The invariant the review checks: every `register-k` event is
-- | matched by exactly one `finalize-k` event, and finalizes run in
-- | LIFO order with respect to registers. This must hold for all
-- | termination modes (success, typed failure, defect, fiber kill).
module Spike.Phase4Review.Stress
  ( Termination(..)
  , runDeepNested
  , runDeepNestedSleep
  ) where

import Prelude

import Data.Array (snoc)
import Effect.Aff (Aff, Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (error)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , addFinalizer
  , ask
  , die
  , fail
  , scoped
  , unsafeRunRIO
  )

-- | How the inner program should end at its chosen depth.
data Termination
  = Succeed
  | TypedFail
  | Defect

-- | Run a nested-scope stress program to `target` depth.
-- |
-- | The events ref captures `register-k` on entry into scope `k` and
-- | `finalize-k` when that scope's finalizer fires.
runDeepNested
  :: Int
  -> Termination
  -> Int
  -> Ref (Array String)
  -> Aff Unit
runDeepNested target termination failAt events = do
  _ <- unsafeRunRIO
    (loop 0 target termination failAt events :: RIO () (stress :: Unit) Unit)
    {}
  pure unit

-- | Variant that sleeps at the innermost level so a forked fiber can
-- | be killed mid-flight by the caller.
runDeepNestedSleep
  :: Int
  -> Milliseconds
  -> Ref (Array String)
  -> Aff Unit
runDeepNestedSleep target sleepFor events = do
  _ <- unsafeRunRIO
    (sleepLoop 0 target sleepFor events :: RIO () () Unit)
    {}
  pure unit

push :: forall r e. Ref (Array String) -> String -> RIO r e Unit
push events s =
  liftAff (liftEffect (Ref.modify_ (\xs -> snoc xs s) events))

loop
  :: forall r
   . Int
  -> Int
  -> Termination
  -> Int
  -> Ref (Array String)
  -> RIO r (stress :: Unit) Unit
loop depth target termination failAt events = scoped do
  scope <- ask (Proxy :: Proxy "scope")
  push events ("register-" <> show depth)
  _ <- addFinalizer scope
    (liftEffect (Ref.modify_ (\xs -> snoc xs ("finalize-" <> show depth)) events))
  if depth == failAt then case termination of
    Succeed -> pure unit
    TypedFail -> fail (Proxy :: Proxy "stress") unit
    Defect -> die (error ("stress-defect-at-" <> show depth))
  else if depth + 1 < target then
    loop (depth + 1) target termination failAt events
  else
    push events "body-reached"

sleepLoop
  :: forall r
   . Int
  -> Int
  -> Milliseconds
  -> Ref (Array String)
  -> RIO r () Unit
sleepLoop depth target sleepFor events = scoped do
  scope <- ask (Proxy :: Proxy "scope")
  push events ("register-" <> show depth)
  _ <- addFinalizer scope
    (liftEffect (Ref.modify_ (\xs -> snoc xs ("finalize-" <> show depth)) events))
  if depth + 1 < target then
    sleepLoop (depth + 1) target sleepFor events
  else do
    push events "sleep-start"
    liftAff (delay sleepFor)
    push events "sleep-end"
