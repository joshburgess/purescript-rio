-- | A virtual-time `Clock` for tests.
-- |
-- | `newTestClock` allocates a clock whose `now` and `sleep` are driven
-- | by an explicit `advance` operation rather than wall-clock time.
-- | Each call to `clock.sleep ms` registers a pending sleeper; the
-- | sleeper resumes when `advance` pushes virtual time past the
-- | sleeper's deadline. Multiple sleepers wake in deadline order
-- | within a single `advance` call.
-- |
-- | The result is a record bundling the `Clock` to inject into a
-- | program with the `advance` controller used by the surrounding
-- | test. A `program` that calls `RIO.Clock.sleep` runs without
-- | waiting on real time; a test can interleave `advance` with
-- | assertions to drive the program through any schedule.
-- |
-- | Cancellation: the sleeper records its tag, so the canceler can
-- | remove it from the pending list when the surrounding fiber is
-- | killed. A canceled sleeper does not fire on subsequent
-- | `advance` calls.
module RIO.Test.Clock
  ( TestClock
  , newTestClock
  ) where

import Prelude

import Data.Array (filter, partition, snoc, sortBy) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref

import RIO.Clock (Clock)

-- | A pending sleeper. `tag` uniquely identifies it (so a canceler
-- | can remove it); `deadlineMs` is the virtual time at which it
-- | should resume; `resume` is the callback installed by `makeAff`.
type Sleeper =
  { tag :: Int
  , deadlineMs :: Number
  , resume :: Either Error Unit -> Effect Unit
  }

-- | A `Clock` paired with the controller used to advance its
-- | virtual time. A test typically holds the whole record, passes
-- | `clock` to the program under test, and calls `advance` from
-- | the test thread.
type TestClock =
  { clock :: Clock
  , advance :: Milliseconds -> Aff Unit
  }

-- | Allocate a fresh test clock starting at virtual time 0.
-- |
-- | Returned in `Aff` so the test can `bind` it alongside the rest
-- | of its setup; the body itself is pure `Effect` work.
-- |
-- | ```purescript
-- | itRIO "fires after a delay" do
-- |   tc <- liftAff newTestClock
-- |   fib <- fork (sleep (Milliseconds 500.0) *> liftAff (record "fired"))
-- |   liftAff (tc.advance (Milliseconds 500.0))
-- |   join fib
-- | ```
newTestClock :: Aff TestClock
newTestClock = liftEffect do
  currentRef <- Ref.new 0.0
  sleepersRef <- Ref.new ([] :: Array Sleeper)
  tagRef <- Ref.new 0
  let
    clockNow :: Aff Milliseconds
    clockNow = liftEffect do
      n <- Ref.read currentRef
      pure (Milliseconds n)

    clockSleep :: Milliseconds -> Aff Unit
    clockSleep (Milliseconds ms) = makeAff \resume -> do
      current <- Ref.read currentRef
      let deadlineMs = current + ms
      if deadlineMs <= current then do
        resume (Right unit)
        pure nonCanceler
      else do
        tag <- Ref.modify (_ + 1) tagRef
        Ref.modify_ (\xs -> Array.snoc xs { tag, deadlineMs, resume })
          sleepersRef
        pure
          ( Canceler \_ -> liftEffect
              (Ref.modify_ (Array.filter (\s -> s.tag /= tag)) sleepersRef)
          )

    advance :: Milliseconds -> Aff Unit
    advance (Milliseconds d) = liftEffect do
      Ref.modify_ (_ + d) currentRef
      current <- Ref.read currentRef
      pending <- Ref.read sleepersRef
      let
        { yes, no } = Array.partition (\s -> s.deadlineMs <= current) pending
        ordered = Array.sortBy (\a b -> compare a.deadlineMs b.deadlineMs) yes
      Ref.write no sleepersRef
      for_ ordered \s -> s.resume (Right unit)

  pure
    { clock: { now: clockNow, sleep: clockSleep }
    , advance
    }
