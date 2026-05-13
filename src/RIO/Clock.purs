-- | A `Clock` service plus a live implementation.
-- |
-- | Phase 7.1 introduces the service in production code (rather than
-- | hiding it in a testing module) so application programs can `ask`
-- | for time and `sleep` through the same row machinery they use for
-- | every other service. The mock implementation lives in
-- | `RIO.Test.Clock`.
-- |
-- | Two operations: `now` returns the current wall-clock time in
-- | milliseconds since the Unix epoch, and `sleep ms` suspends the
-- | current fiber for at least `ms` real-time milliseconds.
-- |
-- | The service operations are `Aff`-valued, following the convention
-- | from `docs/02-services.md`; the smart constructors lift them into
-- | `RIO`.
module RIO.Clock
  ( Clock
  , now
  , sleep
  , liveClock
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Effect.Aff (Aff, Milliseconds)
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

-- | The service record. `now` returns milliseconds since the Unix
-- | epoch; `sleep` blocks for the given duration.
type Clock =
  { now :: Aff Milliseconds
  , sleep :: Milliseconds -> Aff Unit
  }

-- | Read the current wall-clock time.
now :: forall r e. RIO (clock :: Clock | r) e Milliseconds
now = do
  c <- ask (Proxy :: Proxy "clock")
  liftAff c.now

-- | Suspend the current fiber for at least the given duration.
-- |
-- | Under `liveClock` this delegates to `Effect.Aff.delay`, which is
-- | cancellable: an `interrupt` on the fiber will abort the sleep at
-- | the next event-loop tick (see the Phase 0.5 spike's S1).
sleep :: forall r e. Milliseconds -> RIO (clock :: Clock | r) e Unit
sleep ms = do
  c <- ask (Proxy :: Proxy "clock")
  liftAff (c.sleep ms)

-- | A production-ready implementation backed by `Effect.Now` and
-- | `Effect.Aff.delay`. Provide it via `provide` / `provideAll` or
-- | construct a `Layer` that emits it.
liveClock :: Clock
liveClock =
  { now: do
      i <- liftEffect Now.now
      pure (unInstant i)
  , sleep: Aff.delay
  }
