module Test.RIO.ConcurrencySpec (spec) where

import Prelude

import Data.Array (elem, range, snoc) as Array
import Data.Array (range, snoc)
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.DateTime.Instant (unInstant)
import Data.Int (floor) as Int
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay, error, message)
import Effect.Aff (Milliseconds(..)) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , addFinalizer
  , ask
  , die
  , fail
  , fork
  , forkScoped
  , interrupt
  , join
  , parSequence
  , parTraverse
  , parTraverseN
  , race
  , raceAll
  , runRIO
  , sandbox
  , scoped
  , timeout
  , uninterruptible
  , zipPar
  )

spec :: Spec Unit
spec = do
  describe "RIO.Concurrency (Phase 6.1)" do
    describe "fork / join round trip" do
      it "joins a successful fiber and surfaces its value" do
        result <- runRIO do
          fib <- fork (pure 42 :: RIO () () Int)
          join fib
        result `shouldEqual` (Right 42 :: Either _ Int)

      it "lets the parent do other work while the child runs" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Int
          child = do
            liftAff (delay (Milliseconds 5.0))
            liftAff (push "child-done")
            pure 7

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            liftAff (push "parent-mid")
            join fib

        result <- runRIO parent
        result `shouldEqual` (Right 7 :: Either _ Int)
        order <- liftEffect (Ref.read events)
        -- The parent's "parent-mid" must be observed before the
        -- child's "child-done" because of the 5ms delay.
        order `shouldEqual` [ "parent-mid", "child-done" ]

    describe "join surfaces typed failures" do
      it "returns Left on the joiner's row" do
        let
          child :: RIO () (boom :: Unit) Int
          child = fail (Proxy :: Proxy "boom") unit

          parent :: RIO () (boom :: Unit) Int
          parent = do
            fib <- fork child
            join fib
        result <- runRIO parent
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "join surfaces defects via sandbox" do
      it "a die'd fiber surfaces as Aff exception at the join" do
        let
          child :: RIO () () Int
          child = die (error "kaboom")

          parent :: RIO () () (Either _ Int)
          parent = sandbox do
            fib <- fork child
            join fib
        result <- runRIO parent
        case result of
          Right (Left e) -> message e `shouldEqual` "kaboom"
          _ -> 1 `shouldEqual` 0

    describe "interrupt" do
      it "cancels an in-flight Aff.delay and resources are released" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "acquired")
            _ <- addFinalizer scope (push "released")
            liftAff (delay (Milliseconds 5000.0))
            liftAff (push "should-not-happen")

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            -- Join after interrupt: the kill exception propagates as
            -- a defect; sandbox it so the test keeps running.
            _ <- sandbox (join fib)
            pure unit

        result <- runRIO parent
        result `shouldEqual` (Right unit :: Either _ Unit)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquired", "released" ]

      it "is a no-op on an already-completed fiber" do
        let
          child :: RIO () () Int
          child = pure 99

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            n <- join fib
            interrupt fib
            pure n
        result <- runRIO parent
        result `shouldEqual` (Right 99 :: Either _ Int)

    describe "join surfaces interrupt as a defect" do
      it "joining an interrupted fiber throws inside Aff" do
        let
          child :: RIO () () Int
          child = do
            liftAff (delay (Milliseconds 5000.0))
            pure 1

          parent :: RIO () () (Either _ Int)
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 5.0))
            interrupt fib
            sandbox (join fib)
        result <- runRIO parent
        case result of
          Right (Left _) -> pure unit
          _ -> 1 `shouldEqual` 0

    describe "joining twice returns the same result" do
      it "is safe to join an already-joined fiber" do
        let
          child :: RIO () () Int
          child = pure 11

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            n1 <- join fib
            n2 <- join fib
            pure (n1 + n2)
        result <- runRIO parent
        result `shouldEqual` (Right 22 :: Either _ Int)

    describe "parent kill does not finalize child's pre-interrupt state" do
      it "interrupting between acquire and delay releases the resource" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "child:acquired")
            _ <- addFinalizer scope (push "child:released")
            liftAff (delay (Milliseconds 500.0))

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            _ <- sandbox (join fib)
            liftAff (push "parent:after-join")

        result <- runRIO parent
        result `shouldEqual` (Right unit :: Either _ Unit)
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "child:acquired", "child:released", "parent:after-join" ]

  describe "RIO.Concurrency (Phase 6.2)" do
    describe "parTraverse" do
      it "preserves array order in the result" do
        let
          prog :: RIO () () (Array Int)
          prog = parTraverse (\n -> pure (n * n)) (range 1 5)
        result <- runRIO prog
        result `shouldEqual` (Right [ 1, 4, 9, 16, 25 ] :: Either _ (Array Int))

      it "runs two 100ms effects in roughly 100ms, not 200ms" do
        startMs <- liftEffect nowMs
        let
          slow :: Int -> RIO () () Int
          slow n = do
            liftAff (delay (Milliseconds 100.0))
            pure n

          prog :: RIO () () (Array Int)
          prog = parTraverse slow [ 1, 2 ]
        result <- runRIO prog
        endMs <- liftEffect nowMs
        result `shouldEqual` (Right [ 1, 2 ] :: Either _ (Array Int))
        let elapsed = elapsedMs startMs endMs
        -- Parallel should be well under 190ms; sequential would
        -- be 200ms+. The upper bound is generous to absorb CI /
        -- test-framework overhead.
        (elapsed >= 90 && elapsed < 190) `shouldEqual` true

      it "surfaces a typed failure on the parent's row" do
        let
          step :: Int -> RIO () (boom :: Unit) Int
          step n =
            if n == 3 then fail (Proxy :: Proxy "boom") unit
            else pure n

          prog :: RIO () (boom :: Unit) (Array Int)
          prog = parTraverse step [ 1, 2, 3, 4 ]
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "parSequence" do
      it "is parTraverse identity" do
        let
          prog :: RIO () () (Array Int)
          prog = parSequence [ pure 10, pure 20, pure 30 ]
        result <- runRIO prog
        result `shouldEqual` (Right [ 10, 20, 30 ] :: Either _ (Array Int))

    describe "zipPar" do
      it "pairs two successful results" do
        let
          prog :: RIO () () (Tuple Int String)
          prog = zipPar (pure 1) (pure "hi")
        result <- runRIO prog
        result `shouldEqual`
          (Right (Tuple 1 "hi") :: Either _ (Tuple Int String))

      it "associates up to tupling" do
        -- (a `zipPar` b) `zipPar` c  vs.  a `zipPar` (b `zipPar` c)
        -- The pair shapes differ (Tuple (Tuple a b) c vs.
        -- Tuple a (Tuple b c)), but the underlying triple matches.
        let
          a :: RIO () () Int
          a = pure 1

          b :: RIO () () Int
          b = pure 2

          c :: RIO () () Int
          c = pure 3

          lhs :: RIO () () (Tuple (Tuple Int Int) Int)
          lhs = zipPar (zipPar a b) c

          rhs :: RIO () () (Tuple Int (Tuple Int Int))
          rhs = zipPar a (zipPar b c)
        rL <- runRIO lhs
        rR <- runRIO rhs
        case rL, rR of
          Right (Tuple (Tuple x y) z), Right (Tuple x' (Tuple y' z')) ->
            (Tuple (Tuple x y) z) `shouldEqual` (Tuple (Tuple x' y') z')
          _, _ -> 1 `shouldEqual` 0

      it "surfaces the left action's failure first" do
        let
          prog
            :: RIO () (left :: Unit, right :: Unit) (Tuple Int Int)
          prog = zipPar
            (fail (Proxy :: Proxy "left") unit)
            (fail (Proxy :: Proxy "right") unit)
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "two 100ms actions complete in roughly 100ms" do
        startMs <- liftEffect nowMs
        let
          slow :: Int -> RIO () () Int
          slow n = do
            liftAff (delay (Aff.Milliseconds 100.0))
            pure n

          prog :: RIO () () (Tuple Int Int)
          prog = zipPar (slow 1) (slow 2)
        result <- runRIO prog
        endMs <- liftEffect nowMs
        result `shouldEqual`
          (Right (Tuple 1 2) :: Either _ (Tuple Int Int))
        let elapsed = elapsedMs startMs endMs
        (elapsed >= 90 && elapsed < 190) `shouldEqual` true

      it "cancels the slow side when the fast side fails" do
        -- Docstring promise: zipPar shares parTraverse's failure
        -- semantics: "the first Left cancels the other action and
        -- is surfaced on the parent's row." The pinned "surfaces
        -- the left action's failure first" test only checks that
        -- a failure surfaces; the cancellation half is unpinned.
        -- A regression that dropped the `throwError` after the
        -- failure-ref write (or used a non-short-circuiting
        -- applicative) would still surface a Left v but would
        -- let the sibling run to completion. Pin the
        -- cancellation half: a fast-failing left vs a slow
        -- counter bump on the right; after the call returns and
        -- the right's would-be-deadline passes, the counter must
        -- still be 0.
        counter <- liftEffect (Ref.new (0 :: Int))
        let
          leftFast :: RIO () (boom :: Unit) Int
          leftFast = do
            liftAff (delay (Milliseconds 5.0))
            fail (Proxy :: Proxy "boom") unit

          rightSlow :: RIO () (boom :: Unit) Int
          rightSlow = do
            liftAff (delay (Milliseconds 100.0))
            liftEffect (Ref.modify_ (_ + 1) counter)
            pure 1

          prog :: RIO () (boom :: Unit) (Tuple Int Int)
          prog = zipPar leftFast rightSlow
        _ <- runRIO prog
        liftAff (delay (Milliseconds 150.0))
        seen <- liftEffect (Ref.read counter)
        seen `shouldEqual` 0

  describe "RIO.Concurrency (Phase 6.3)" do
    describe "race" do
      it "the faster branch wins" do
        let
          fast :: RIO () () Int
          fast = do
            liftAff (delay (Milliseconds 10.0))
            pure 1

          slow :: RIO () () Int
          slow = do
            liftAff (delay (Milliseconds 500.0))
            pure 2

          prog :: RIO () () Int
          prog = race fast slow
        result <- runRIO prog
        result `shouldEqual` (Right 1 :: Either _ Int)

      it "the loser's resources are released by interrupt" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          loser :: RIO () () Int
          loser = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "loser:acquired")
            _ <- addFinalizer scope (push "loser:released")
            liftAff (delay (Milliseconds 500.0))
            pure 99

          winner :: RIO () () Int
          winner = do
            liftAff (delay (Milliseconds 10.0))
            pure 1

          prog :: RIO () () Int
          prog = race winner loser
        result <- runRIO prog
        result `shouldEqual` (Right 1 :: Either _ Int)
        -- Give the runtime a moment to drain finalizers on the loser
        -- side, then read the event log.
        liftAff (delay (Milliseconds 20.0))
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "loser:acquired", "loser:released" ]

      it "a fast failure can win the race" do
        let
          fastFail :: RIO () (boom :: Unit) Int
          fastFail = do
            liftAff (delay (Milliseconds 10.0))
            fail (Proxy :: Proxy "boom") unit

          slowSuccess :: RIO () (boom :: Unit) Int
          slowSuccess = do
            liftAff (delay (Milliseconds 500.0))
            pure 1

          prog :: RIO () (boom :: Unit) Int
          prog = race fastFail slowSuccess
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "raceAll" do
      it "the fastest of three wins" do
        let
          a :: RIO () () Int
          a = do
            liftAff (delay (Milliseconds 100.0))
            pure 1

          b :: RIO () () Int
          b = do
            liftAff (delay (Milliseconds 10.0))
            pure 2

          c :: RIO () () Int
          c = do
            liftAff (delay (Milliseconds 50.0))
            pure 3

          prog :: RIO () () Int
          prog = raceAll
            (NEArray.cons' a [ b, c ])
        result <- runRIO prog
        result `shouldEqual` (Right 2 :: Either _ Int)

      it "all losers' resources are released" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          loser :: String -> RIO () () Int
          loser tag = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push (tag <> ":acquired"))
            _ <- addFinalizer scope (push (tag <> ":released"))
            liftAff (delay (Milliseconds 500.0))
            pure 0

          winner :: RIO () () Int
          winner = do
            liftAff (delay (Milliseconds 10.0))
            pure 1

          prog :: RIO () () Int
          prog = raceAll
            (NEArray.cons' winner [ loser "A", loser "B" ])
        result <- runRIO prog
        result `shouldEqual` (Right 1 :: Either _ Int)
        liftAff (delay (Milliseconds 30.0))
        order <- liftEffect (Ref.read events)
        -- We don't assert order between A and B (the runtime may
        -- start them in either order); we do assert each one's
        -- acquire-release pair is present.
        Array.elem "A:released" order `shouldEqual` true
        Array.elem "B:released" order `shouldEqual` true

  describe "RIO.Concurrency extras" do
    describe "parTraverse short-circuit" do
      it "interrupts sibling fibers on the first failure" do
        completed <- liftEffect (Ref.new (0 :: Int))
        let
          step :: Int -> RIO () (boom :: Unit) Int
          step n =
            if n == 0 then do
              liftAff (delay (Milliseconds 5.0))
              fail (Proxy :: Proxy "boom") unit
            else do
              liftAff (delay (Milliseconds 200.0))
              liftEffect (Ref.modify_ (_ + 1) completed)
              pure n

          prog :: RIO () (boom :: Unit) (Array Int)
          prog = parTraverse step [ 0, 1, 2, 3 ]
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        -- Wait for what would have been the full 200ms run, then
        -- read the counter. Siblings should have been interrupted
        -- before their delay completed.
        liftAff (delay (Milliseconds 250.0))
        n <- liftEffect (Ref.read completed)
        n `shouldEqual` 0

      it "a defect in one branch propagates via sandbox and interrupts siblings" do
        -- Docstring promise: "Defects from any branch propagate as
        -- Aff defects (observable via `RIO.Error.sandbox`); they
        -- also interrupt the siblings." Pin both halves: (1) the
        -- defect surfaces through `sandbox` as Left Error rather
        -- than collapsing into a typed failure, and (2) sibling
        -- branches do not complete their 200ms work, mirroring
        -- the typed-failure short-circuit test above.
        completed <- liftEffect (Ref.new (0 :: Int))
        let
          step :: Int -> RIO () () Int
          step n =
            if n == 0 then do
              liftAff (delay (Milliseconds 5.0))
              die (error "kaboom")
            else do
              liftAff (delay (Milliseconds 200.0))
              liftEffect (Ref.modify_ (_ + 1) completed)
              pure n

          prog :: RIO () () (Either _ (Array Int))
          prog = sandbox (parTraverse step [ 0, 1, 2, 3 ])
        result <- runRIO prog
        case result of
          Right (Left e) -> message e `shouldEqual` "kaboom"
          _ -> 1 `shouldEqual` 0
        liftAff (delay (Milliseconds 250.0))
        n <- liftEffect (Ref.read completed)
        n `shouldEqual` 0

    describe "parTraverseN" do
      it "preserves array order in the result" do
        let
          prog :: RIO () () (Array Int)
          prog = parTraverseN 3 (\n -> pure (n + 100)) (range 1 5)
        result <- runRIO prog
        result `shouldEqual`
          (Right [ 101, 102, 103, 104, 105 ] :: Either _ (Array Int))

      it "caps concurrency: never more than N in flight" do
        inflight <- liftEffect (Ref.new (0 :: Int))
        peak <- liftEffect (Ref.new (0 :: Int))
        let
          step :: Int -> RIO () () Int
          step n = do
            cur <- liftEffect
              (Ref.modify (_ + 1) inflight)
            liftEffect
              ( Ref.modify_
                  (\p -> if cur > p then cur else p)
                  peak
              )
            liftAff (delay (Milliseconds 30.0))
            _ <- liftEffect (Ref.modify (\x -> x - 1) inflight)
            pure n

          prog :: RIO () () (Array Int)
          prog = parTraverseN 2 step (range 1 6)
        result <- runRIO prog
        result `shouldEqual`
          (Right [ 1, 2, 3, 4, 5, 6 ] :: Either _ (Array Int))
        peakSeen <- liftEffect (Ref.read peak)
        (peakSeen <= 2) `shouldEqual` true

      it "n <= 1 runs sequentially (concurrency capped at 1)" do
        inflight <- liftEffect (Ref.new (0 :: Int))
        peak <- liftEffect (Ref.new (0 :: Int))
        let
          step :: Int -> RIO () () Int
          step n = do
            cur <- liftEffect (Ref.modify (_ + 1) inflight)
            liftEffect (Ref.modify_ (\p -> if cur > p then cur else p) peak)
            liftAff (delay (Milliseconds 10.0))
            _ <- liftEffect (Ref.modify (\x -> x - 1) inflight)
            pure n

          prog :: RIO () () (Array Int)
          prog = parTraverseN 1 step [ 1, 2, 3 ]
        _ <- runRIO prog
        peakSeen <- liftEffect (Ref.read peak)
        peakSeen `shouldEqual` 1

      it "n = 0 is treated as 1 (sequential), not as zero-size chunks" do
        -- Docstring promise: "`n <= 0` is treated as `1`
        -- (sequential)." The pinned `n <= 1` test only covers
        -- the upper bound (`n = 1`) of that interval; the
        -- zero/negative half is unpinned. The implementation
        -- guards with `if n <= 1 then 1 else n`, which feeds
        -- into `chunksOf size as`; `chunksOf 0 _` would
        -- recursively call itself with the same array (after a
        -- zero-width take/drop) and loop forever. A regression
        -- that tightened the guard to `n < 1` would silently
        -- hang on `parTraverseN 0`. Pin the guard with `n = 0`:
        -- the call must complete, return correct values, and
        -- observe peak concurrency of 1.
        inflight <- liftEffect (Ref.new (0 :: Int))
        peak <- liftEffect (Ref.new (0 :: Int))
        let
          step :: Int -> RIO () () Int
          step n = do
            cur <- liftEffect (Ref.modify (_ + 1) inflight)
            liftEffect (Ref.modify_ (\p -> if cur > p then cur else p) peak)
            liftAff (delay (Milliseconds 5.0))
            _ <- liftEffect (Ref.modify (\x -> x - 1) inflight)
            pure (n * 10)

          prog :: RIO () () (Array Int)
          prog = parTraverseN 0 step [ 1, 2, 3 ]
        result <- runRIO prog
        peakSeen <- liftEffect (Ref.read peak)
        case result of
          Right xs -> xs `shouldEqual` [ 10, 20, 30 ]
          Left _ -> 1 `shouldEqual` 0
        peakSeen `shouldEqual` 1

      it "a typed failure in one chunk aborts the remaining chunks" do
        -- Docstring promise: "the first typed failure inside a
        -- chunk cancels its siblings and aborts the remaining
        -- chunks." With `parTraverseN 2` over `[1..6]` the input
        -- splits into chunks `[1,2]`, `[3,4]`, `[5,6]`. If item
        -- `2` fails inside the first chunk, nothing in chunks
        -- `[3,4]` or `[5,6]` should ever start.
        started <- liftEffect (Ref.new [])
        let
          push :: Int -> RIO () (boom :: Unit) Unit
          push n =
            liftEffect (Ref.modify_ (\xs -> snoc xs n) started)

          step :: Int -> RIO () (boom :: Unit) Int
          step n
            | n == 2 = do
                push n
                liftAff (delay (Milliseconds 10.0))
                fail (Proxy :: Proxy "boom") unit
            | otherwise = do
                push n
                liftAff (delay (Milliseconds 5.0))
                pure n

          prog :: RIO () (boom :: Unit) (Array Int)
          prog = parTraverseN 2 step (range 1 6)
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        liftAff (delay (Milliseconds 50.0))
        seen <- liftEffect (Ref.read started)
        (3 `Array.elem` seen) `shouldEqual` false
        (4 `Array.elem` seen) `shouldEqual` false
        (5 `Array.elem` seen) `shouldEqual` false
        (6 `Array.elem` seen) `shouldEqual` false

    describe "timeout" do
      it "returns Just on success when the action beats the deadline" do
        let
          prog :: RIO () () (Maybe Int)
          prog = timeout (Milliseconds 200.0) do
            liftAff (delay (Milliseconds 10.0))
            pure 42
        result <- runRIO prog
        result `shouldEqual` (Right (Just 42) :: Either _ (Maybe Int))

      it "returns Nothing when the deadline fires first" do
        let
          prog :: RIO () () (Maybe Int)
          prog = timeout (Milliseconds 10.0) do
            liftAff (delay (Milliseconds 500.0))
            pure 42
        result <- runRIO prog
        result `shouldEqual` (Right Nothing :: Either _ (Maybe Int))

      it "releases resources when the deadline interrupts the action" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          slow :: RIO () () Int
          slow = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "acquired")
            _ <- addFinalizer scope (push "released")
            liftAff (delay (Milliseconds 500.0))
            pure 1

          prog :: RIO () () (Maybe Int)
          prog = timeout (Milliseconds 10.0) slow
        result <- runRIO prog
        result `shouldEqual` (Right Nothing :: Either _ (Maybe Int))
        liftAff (delay (Milliseconds 30.0))
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquired", "released" ]

      it "typed failures surface unchanged on the parent's row" do
        let
          prog :: RIO () (boom :: Unit) (Maybe Int)
          prog = timeout (Milliseconds 500.0)
            (fail (Proxy :: Proxy "boom") unit)
        result <- runRIO prog
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "uninterruptible" do
      it "surfaces a typed failure raised inside the block on the parent's row" do
        -- The existing test pins that uninterruptible completes
        -- a critical section despite an interrupt. The failure
        -- path was not pinned: a typed failure raised inside
        -- the protected region should still surface to the
        -- caller. (uninterruptible only blocks kills, not the
        -- typed-error channel.)
        let
          program :: RIO () (boom :: Unit) Int
          program = uninterruptible (fail (Proxy :: Proxy "boom") unit)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "completes a critical section even after an interrupt is sent" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = uninterruptible do
            liftAff (push "start")
            liftAff (delay (Milliseconds 50.0))
            liftAff (push "end")

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            _ <- sandbox (join fib)
            pure unit

        _ <- runRIO parent
        liftAff (delay (Milliseconds 100.0))
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "start", "end" ]

      it "a queued interrupt fires once the uninterruptible region exits" do
        -- Docstring promise: "any `interrupt` sent to the
        -- enclosing fiber is queued; it fires only after the
        -- region completes." The pinned tests above check that
        -- the region itself completes despite the interrupt;
        -- pin the second half by adding a post-region statement
        -- and asserting it never runs because the queued
        -- interrupt landed at the region boundary.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = do
            uninterruptible do
              liftAff (push "before-protected")
              liftAff (delay (Milliseconds 50.0))
              liftAff (push "after-protected")
            liftAff (push "after-uninterruptible")

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            _ <- sandbox (join fib)
            pure unit
        _ <- runRIO parent
        liftAff (delay (Milliseconds 100.0))
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "before-protected", "after-protected" ]

    describe "forkScoped" do
      it "interrupts the fiber when the scope exits" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          worker :: forall r. RIO r () Unit
          worker = do
            liftAff (push "worker:start")
            liftAff (delay (Milliseconds 500.0))
            liftAff (push "worker:should-not-fire")

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- forkScoped scope worker
            liftAff (delay (Milliseconds 20.0))
            liftAff (push "scope:exit")

        result <- runRIO program
        result `shouldEqual` (Right unit :: Either _ Unit)
        liftAff (delay (Milliseconds 80.0))
        order <- liftEffect (Ref.read events)
        Array.elem "worker:start" order `shouldEqual` true
        Array.elem "scope:exit" order `shouldEqual` true
        Array.elem "worker:should-not-fire" order `shouldEqual` false

      it "interrupts the fiber when the scope exits with a typed failure" do
        -- `forkScoped`'s docstring promises that the child is
        -- interrupted "when the scope exits (success, typed
        -- failure, defect, or kill), an `interrupt` is sent to
        -- the fiber as part of the scope's LIFO finalizer
        -- pass." The existing test above only covers the
        -- success path. If `forkScoped` were refactored to
        -- wrap cleanup in `Aff.finally` around the inner
        -- body rather than register it on the scope, the
        -- finalizer would still fire on success but would be
        -- bypassed when the scope body exits via a typed
        -- failure (which surfaces as `Left v` from the
        -- bracket release rather than as an Aff exception).
        -- Pin the typed-failure exit path so all four
        -- termination paths the docstring mentions are
        -- documented in tests.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          worker :: forall r. RIO r () Unit
          worker = do
            liftAff (push "worker:start")
            liftAff (delay (Milliseconds 500.0))
            liftAff (push "worker:should-not-fire")

          program :: RIO () (boom :: Unit) Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- forkScoped scope worker
            liftAff (delay (Milliseconds 20.0))
            fail (Proxy :: Proxy "boom") unit

        _ <- runRIO program
        liftAff (delay (Milliseconds 80.0))
        order <- liftEffect (Ref.read events)
        Array.elem "worker:start" order `shouldEqual` true
        Array.elem "worker:should-not-fire" order `shouldEqual` false

      it "interrupts the fiber when the scope exits with a defect" do
        -- Third of `forkScoped`'s four advertised termination
        -- paths: "success, typed failure, defect, or kill". The
        -- success and typed-failure paths are pinned above; this
        -- pin covers the defect path. A scope body that `die`s
        -- collapses into an Aff exception during the bracket
        -- release, and the finalizer chain still runs LIFO. A
        -- regression that special-cased only the "Right v" exit
        -- of the scope body (and skipped registered finalizers
        -- when the body threw) would still pass the success and
        -- typed-failure pins but would let the worker delay run
        -- to completion here.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          worker :: forall r. RIO r () Unit
          worker = do
            liftAff (push "worker:start")
            liftAff (delay (Milliseconds 500.0))
            liftAff (push "worker:should-not-fire")

          program :: RIO () () (Either _ Unit)
          program = sandbox $ scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- forkScoped scope worker
            liftAff (delay (Milliseconds 20.0))
            die (error "scope-defect")

        result <- runRIO program
        case result of
          Right (Left e) -> message e `shouldEqual` "scope-defect"
          _ -> 1 `shouldEqual` 0
        liftAff (delay (Milliseconds 80.0))
        order <- liftEffect (Ref.read events)
        Array.elem "worker:start" order `shouldEqual` true
        Array.elem "worker:should-not-fire" order `shouldEqual` false

      it "interrupts the fiber when the outer scope-bearing fiber is killed" do
        -- Final advertised termination path: "kill". The scope's
        -- finalizer must fire when the fiber owning the scope is
        -- itself killed (not just when the scope body completes
        -- or throws). Set this up by forking the `scoped` block
        -- into an outer fiber and interrupting it mid-flight.
        -- A regression that attached the worker-kill finalizer
        -- via `Aff.finally` around the inner scope body would
        -- skip the cleanup when the outer fiber is killed (the
        -- body is interrupted partway through, never reaching
        -- the finally hook); registering on the scope and
        -- letting the runtime run finalizers on kill is the
        -- only path that survives this test.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          worker :: forall r. RIO r () Unit
          worker = do
            liftAff (push "worker:start")
            liftAff (delay (Milliseconds 500.0))
            liftAff (push "worker:should-not-fire")

          outer :: RIO () () Unit
          outer = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- forkScoped scope worker
            liftAff (delay (Milliseconds 500.0))
            liftAff (push "outer:should-not-fire")

          program :: RIO () () Unit
          program = do
            fib <- fork outer
            liftAff (delay (Milliseconds 20.0))
            interrupt fib
            _ <- sandbox (join fib)
            pure unit

        _ <- runRIO program
        liftAff (delay (Milliseconds 100.0))
        order <- liftEffect (Ref.read events)
        Array.elem "worker:start" order `shouldEqual` true
        Array.elem "worker:should-not-fire" order `shouldEqual` false
        Array.elem "outer:should-not-fire" order `shouldEqual` false
  where
  nowMs = do
    instant <- Now.now
    pure (unwrap (unInstant instant))

  elapsedMs :: Number -> Number -> Int
  elapsedMs startN endN = Int.floor (endN - startN)
