-- | Phase 6 review stress scenarios.
-- |
-- | Each scenario exercises one concurrency combinator under random
-- | scheduling and asserts a single load-bearing invariant: the
-- | resource counter must return to zero on every termination path.
-- | A non-zero counter means a finalizer leaked.
-- |
-- | The scenarios are intentionally narrow and fast (each runs in
-- | well under 100ms). The harness in `Main` calls each one in a
-- | loop with randomised parameters, summing leaks across runs.
module Spike.Phase6Review.Stress
  ( ScenarioResult
  , interruptScenario
  , parTraverseScenario
  , raceScenario
  , zipParScenario
  ) where

import Prelude

import Data.Array (range) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Int (toNumber)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Core
  ( RIO
  , acquireRelease
  , addFinalizer
  , ask
  , fail
  , fork
  , interrupt
  , parTraverse
  , raceAll
  , scoped
  , unsafeRunRIO
  , zipPar
  )

-- | Outcome of a single iteration: whether the counter returned to
-- | zero, with a short reason on failure.
type ScenarioResult =
  { ok :: Boolean
  , leaked :: Int
  }

-- | parTraverse over `count` actions, each acquiring a resource,
-- | delaying randomly within `[0, maxDelayMs]`, and failing with
-- | a typed error with probability `failPct%`. The invariant: after
-- | the parTraverse completes (with or without a typed failure on
-- | the parent's row), every acquire has been matched by a release.
parTraverseScenario
  :: { count :: Int
     , maxDelayMs :: Int
     , failPct :: Int
     }
  -> Aff ScenarioResult
parTraverseScenario opts = do
  counter <- liftEffect (Ref.new 0)
  let
    action :: Int -> RIO () (boom :: Int) Int
    action i = acquireRelease
      (liftEffect (Ref.modify_ (_ + 1) counter) *> pure i)
      (\_ -> liftEffect (Ref.modify_ (_ - 1) counter))
      ( \_ -> do
          d <- liftEffect (randomInt 0 opts.maxDelayMs)
          f <- liftEffect (randomInt 0 99)
          liftAff (delay (Milliseconds (toNumber d)))
          if f < opts.failPct then fail (Proxy :: Proxy "boom") i
          else pure i
      )

    program :: RIO () (boom :: Int) (Array Int)
    program = parTraverse action (Array.range 1 opts.count)

  _ <- attempt (unsafeRunRIO program {})
  leaked <- liftEffect (Ref.read counter)
  pure { ok: leaked == 0, leaked }

-- | zipPar over two acquire/release actions with random delays.
-- | Each side independently fails with probability `failPct%`. The
-- | invariant: counter returns to zero whether neither, one, or both
-- | fail.
zipParScenario
  :: { maxDelayMs :: Int
     , failPct :: Int
     }
  -> Aff ScenarioResult
zipParScenario opts = do
  counter <- liftEffect (Ref.new 0)
  let
    mk :: RIO () (boom :: Unit) Unit
    mk = acquireRelease
      (liftEffect (Ref.modify_ (_ + 1) counter))
      (\_ -> liftEffect (Ref.modify_ (_ - 1) counter))
      ( \_ -> do
          d <- liftEffect (randomInt 0 opts.maxDelayMs)
          f <- liftEffect (randomInt 0 99)
          liftAff (delay (Milliseconds (toNumber d)))
          if f < opts.failPct then fail (Proxy :: Proxy "boom") unit
          else pure unit
      )

    program :: RIO () (boom :: Unit) _
    program = zipPar mk mk

  _ <- attempt (unsafeRunRIO program {})
  leaked <- liftEffect (Ref.read counter)
  pure { ok: leaked == 0, leaked }

-- | race over `count` actions, each acquiring a resource and
-- | delaying within `[1, maxDelayMs]`. The fastest wins; the losers
-- | are interrupted by Aff. The invariant: every loser's resource
-- | is released by the time `raceAll` returns.
raceScenario
  :: { count :: Int
     , maxDelayMs :: Int
     }
  -> Aff ScenarioResult
raceScenario opts = do
  counter <- liftEffect (Ref.new 0)
  let
    action :: Int -> RIO () () Int
    action i = acquireRelease
      (liftEffect (Ref.modify_ (_ + 1) counter) *> pure i)
      (\_ -> liftEffect (Ref.modify_ (_ - 1) counter))
      ( \_ -> do
          d <- liftEffect (randomInt 1 opts.maxDelayMs)
          liftAff (delay (Milliseconds (toNumber d)))
          pure i
      )

    arr :: NonEmptyArray (RIO () () Int)
    arr = NEArray.cons' (action 0)
      (map action (Array.range 1 (opts.count - 1)))

    program :: RIO () () Int
    program = raceAll arr

  _ <- attempt (unsafeRunRIO program {})
  leaked <- liftEffect (Ref.read counter)
  pure { ok: leaked == 0, leaked }

-- | fork a chain of `depth` nested `scoped` blocks, each registering
-- | a finalizer that decrements the counter. The innermost sleeps;
-- | after `killAfterMs` the parent interrupts the fiber. The
-- | invariant: every registered finalizer fires, returning the
-- | counter to zero.
interruptScenario
  :: { depth :: Int
     , sleepMs :: Int
     , killAfterMs :: Int
     }
  -> Aff ScenarioResult
interruptScenario opts = do
  counter <- liftEffect (Ref.new 0)
  let
    nested :: forall r. Int -> RIO r () Unit
    nested n =
      if n <= 0 then liftAff (delay (Milliseconds (toNumber opts.sleepMs)))
      else scoped do
        scope <- ask (Proxy :: Proxy "scope")
        liftEffect (Ref.modify_ (_ + 1) counter)
        _ <- addFinalizer scope
          (liftEffect (Ref.modify_ (_ - 1) counter))
        nested (n - 1)

    program :: RIO () () Unit
    program = do
      fib <- fork (nested opts.depth)
      liftAff (delay (Milliseconds (toNumber opts.killAfterMs)))
      interrupt fib

  _ <- attempt (unsafeRunRIO program {})
  leaked <- liftEffect (Ref.read counter)
  pure { ok: leaked == 0, leaked }
