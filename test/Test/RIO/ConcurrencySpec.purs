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

  describe "RIO.Concurrency (v0.2)" do
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
  where
  nowMs = do
    instant <- Now.now
    pure (unwrap (unInstant instant))

  elapsedMs :: Number -> Number -> Int
  elapsedMs startN endN = Int.floor (endN - startN)
