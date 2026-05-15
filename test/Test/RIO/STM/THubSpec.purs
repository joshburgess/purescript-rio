module Test.RIO.STM.THubSpec (spec) where

import Prelude hiding (join)

import Data.Array (range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, fork, join, runRIO, runRIO')
import RIO.STM (atomically)
import RIO.STM.THub
  ( Strategy(..)
  , THub
  , isEmptySubscription
  , lengthSubscription
  , newBoundedTHub
  , newDroppingTHub
  , newSlidingTHub
  , newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , subscriberCount
  , takeSubscription
  , tryTakeSubscription
  , unsubscribeTHub
  , withSubscription
  )

spec :: Spec Unit
spec = describe "RIO.STM.THub" do
  describe "Strategy instances" do
    -- `Strategy`'s `Show` instance is hand-written (case _ of
    -- Bounded n -> "Bounded " <> show n ...), parallel to
    -- `SpanStatus`'s pinned Show in TracerSpec, `LogLevel`'s in
    -- LoggerSpec, and `MetricKind`'s in MetricsSpec. Every other
    -- test in this suite constructs a Strategy via the smart
    -- constructors (newBoundedTHub etc.) and never inspects `show
    -- strategy`, so a regression that namespaced the constructors
    -- ("Strategy.Bounded 5") or accidentally swapped `Unbounded ->
    -- "Unbounded"` for `Unbounded -> "unbounded"` would compile
    -- and pass every existing test; only a downstream log line
    -- or exporter that relied on `show strategy` to label the
    -- back-pressure policy would silently break. Pin the four
    -- rendered constructors directly so that channel is protected.
    it "Show renders each strategy by its constructor name" do
      show (Bounded 5) `shouldEqual` "Bounded 5"
      show (Sliding 7) `shouldEqual` "Sliding 7"
      show (Dropping 3) `shouldEqual` "Dropping 3"
      show Unbounded `shouldEqual` "Unbounded"

  describe "subscribers and basic delivery" do
    it "a single subscriber receives every value in publish order" do
      let
        program :: RIO () () (Array Int)
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          traverse_ (\n -> atomically (publishTHub hub n) >>= \_ -> pure unit)
            [ 1, 2, 3 ]
          traverse (\_ -> atomically (takeSubscription sub)) [ 1, 2, 3 ]
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "two subscribers each receive every value independently" do
      let
        program :: RIO () () { a :: Array Int, b :: Array Int }
        program = do
          hub <- atomically newUnboundedTHub
          sa <- atomically (subscribeTHub hub)
          sb <- atomically (subscribeTHub hub)
          traverse_ (\n -> atomically (publishTHub hub n) >>= \_ -> pure unit)
            [ 10, 20, 30 ]
          a <- traverse (\_ -> atomically (takeSubscription sa)) [ 10, 20, 30 ]
          b <- traverse (\_ -> atomically (takeSubscription sb)) [ 10, 20, 30 ]
          pure { a, b }
      result <- runRIO' program
      result `shouldEqual` { a: [ 10, 20, 30 ], b: [ 10, 20, 30 ] }

    it "a subscriber sees only values published after it registers" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          hub <- atomically newUnboundedTHub
          _ <- atomically (publishTHub hub 1)
          sub <- atomically (subscribeTHub hub)
          atomically (tryTakeSubscription sub)
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "subscriberCount tracks subscribe and unsubscribe" do
      let
        program :: RIO () () { c0 :: Int, c1 :: Int, c2 :: Int, c1again :: Int }
        program = do
          (hub :: THub Int) <- atomically newUnboundedTHub
          c0 <- atomically (subscriberCount hub)
          a <- atomically (subscribeTHub hub)
          c1 <- atomically (subscriberCount hub)
          b <- atomically (subscribeTHub hub)
          c2 <- atomically (subscriberCount hub)
          atomically (unsubscribeTHub b)
          c1again <- atomically (subscriberCount hub)
          atomically (unsubscribeTHub a)
          pure { c0, c1, c2, c1again }
      result <- runRIO' program
      result `shouldEqual` { c0: 0, c1: 1, c2: 2, c1again: 1 }

  describe "Sliding strategy" do
    it "drops the oldest value when the buffer is full" do
      let
        program :: RIO () () (Array Int)
        program = do
          hub <- atomically (newSlidingTHub 2)
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (publishTHub hub 2)
          _ <- atomically (publishTHub hub 3)
          traverse (\_ -> atomically (takeSubscription sub)) [ 0, 0 ]
      result <- runRIO' program
      result `shouldEqual` [ 2, 3 ]

    it "consecutive overflows drop oldest-first across the whole sequence" do
      -- Docstring promise: a subscriber's buffer "loses its oldest
      -- entry" when full. The existing "drops the oldest value
      -- when the buffer is full" test publishes 3 values into cap
      -- 2 and observes `[2, 3]`, which only pins a single drop.
      -- A regression that drops from a fixed buffer offset (e.g.,
      -- always discarding index 0 of the original buffer rather
      -- than re-reading the head after each drop, or shifting an
      -- index without modular reduction in a ring buffer) would
      -- pass the single-overflow case but yield `[2, 4]` instead
      -- of `[3, 4]` for two consecutive overflows. Pin two
      -- consecutive overflows into cap 2 with publishes
      -- `[1, 2, 3, 4]`: the surviving buffer must be `[3, 4]`.
      let
        program :: RIO () () (Array Int)
        program = do
          hub <- atomically (newSlidingTHub 2)
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (publishTHub hub 2)
          _ <- atomically (publishTHub hub 3)
          _ <- atomically (publishTHub hub 4)
          traverse (\_ -> atomically (takeSubscription sub)) [ 0, 0 ]
      result <- runRIO' program
      result `shouldEqual` [ 3, 4 ]

    it "publishTHub always returns true for sliding" do
      let
        program :: RIO () () { r1 :: Boolean, r2 :: Boolean, r3 :: Boolean }
        program = do
          hub <- atomically (newSlidingTHub 1)
          _ <- atomically (subscribeTHub hub)
          r1 <- atomically (publishTHub hub 10)
          r2 <- atomically (publishTHub hub 20)
          r3 <- atomically (publishTHub hub 30)
          pure { r1, r2, r3 }
      result <- runRIO' program
      result `shouldEqual` { r1: true, r2: true, r3: true }

  describe "Dropping strategy" do
    it "drops the new value and returns false when the buffer is full" do
      let
        program
          :: RIO () ()
               { r1 :: Boolean
               , r2 :: Boolean
               , r3 :: Boolean
               , buffered :: Array Int
               }
        program = do
          hub <- atomically (newDroppingTHub 2)
          sub <- atomically (subscribeTHub hub)
          r1 <- atomically (publishTHub hub 1)
          r2 <- atomically (publishTHub hub 2)
          r3 <- atomically (publishTHub hub 3)
          buffered <- traverse (\_ -> atomically (takeSubscription sub)) [ 0, 0 ]
          pure { r1, r2, r3, buffered }
      result <- runRIO' program
      result `shouldEqual`
        { r1: true, r2: true, r3: false, buffered: [ 1, 2 ] }

    it "returns false when ONE of several subscribers drops; others still receive" do
      -- Docstring promise: `publishTHub` returns "`false` if at
      -- least one dropped it", and for `Dropping n`:
      -- "subscribers whose buffer was full drop the new value"
      -- (i.e., non-full subscribers still accept). The existing
      -- Dropping test uses a single subscriber, so the
      -- "partial-drop" branch (one subscriber drops, another
      -- accepts) is unpinned: a regression that broke the
      -- per-subscriber independence (e.g., aborting the whole
      -- fold on the first drop) would still pass the
      -- single-subscriber test. Pin both halves with two
      -- subscribers at cap 1: drain one so only its peer is
      -- full at publish time, then assert the publish returns
      -- `false` AND the drained subscriber still received the
      -- new value.
      let
        program
          :: RIO () ()
               { r2 :: Boolean
               , fastValue :: Maybe Int
               , slowValue :: Maybe Int
               , slowAfter :: Maybe Int
               }
        program = do
          hub <- atomically (newDroppingTHub 1)
          slow <- atomically (subscribeTHub hub)
          fast <- atomically (subscribeTHub hub)
          -- Both accept the first value; buffers are now full.
          _ <- atomically (publishTHub hub 1)
          -- Drain only `fast`; `slow` is still full at cap.
          _ <- atomically (takeSubscription fast)
          -- `slow` is full and must drop; `fast` has a slot and
          -- must accept. The whole publish must report `false`.
          r2 <- atomically (publishTHub hub 2)
          -- `fast` should now hold the new value (2).
          fastValue <- atomically (tryTakeSubscription fast)
          -- `slow` should still hold only the original (1) — the
          -- second publish was dropped, not overwritten.
          slowValue <- atomically (tryTakeSubscription slow)
          slowAfter <- atomically (tryTakeSubscription slow)
          pure { r2, fastValue, slowValue, slowAfter }
      result <- runRIO' program
      result `shouldEqual`
        { r2: false
        , fastValue: Just 2
        , slowValue: Just 1
        , slowAfter: Nothing
        }

  describe "Bounded strategy" do
    it "publishTHub blocks (retries) until a consumer takes a value" do
      events <- liftEffect (Ref.new [])
      let
        push :: forall r e. String -> RIO r e Unit
        push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

        program :: RIO () () Int
        program = do
          hub <- atomically (newBoundedTHub 1)
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          push "buffer-full"
          producer <- fork do
            _ <- atomically (publishTHub hub 2)
            push "publish-2-committed"
            pure unit
          liftAff (delay (Milliseconds 20.0))
          push "before-take"
          v1 <- atomically (takeSubscription sub)
          _ <- join producer
          v2 <- atomically (takeSubscription sub)
          pure (v1 + v2)
      result <- runRIO' program
      result `shouldEqual` 3
      order <- liftEffect (Ref.read events)
      order `shouldEqual`
        [ "buffer-full", "before-take", "publish-2-committed" ]

    it "publishTHub blocks while ANY subscriber is full; slowest dictates throughput" do
      -- Docstring promise (lines 139-140 of THub.purs): for
      -- `Bounded n`, `publishTHub` "retries while any subscriber
      -- buffer is full". The pinned single-subscriber test
      -- covers only the `n=1` "buffer-full" case; the
      -- multi-subscriber "slowest subscriber dictates throughput"
      -- contract is unpinned. A regression that checked only the
      -- first subscriber's buffer (a missing iteration, an early
      -- exit, or an `Array.any` swapped with `Array.head`) would
      -- still pass single-subscriber tests but would let the
      -- producer commit while a slow peer is still full. Pin it:
      -- with two subscribers at cap 1, drain only the fast one
      -- after the first publish, fork a second publish, and
      -- assert it has NOT committed after a settle delay. Then
      -- drain the slow peer and assert the second publish
      -- commits and both peers receive value 2 in order.
      events <- liftEffect (Ref.new [])
      let
        push :: forall r e. String -> RIO r e Unit
        push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

        program :: RIO () () { fast :: Int, slow :: Int }
        program = do
          hub <- atomically (newBoundedTHub 1)
          slow <- atomically (subscribeTHub hub)
          fast <- atomically (subscribeTHub hub)
          -- Both subscribers are now full at cap 1.
          _ <- atomically (publishTHub hub 1)
          -- Drain ONLY the fast subscriber; slow is still full.
          _ <- atomically (takeSubscription fast)
          push "fast-drained"
          producer <- fork do
            _ <- atomically (publishTHub hub 2)
            push "publish-2-committed"
            pure unit
          -- Producer should still be retrying because `slow` is
          -- full, even though `fast` has a slot.
          liftAff (delay (Milliseconds 20.0))
          push "before-slow-drain"
          slowFirst <- atomically (takeSubscription slow)
          _ <- join producer
          push "after-join"
          fast2 <- atomically (takeSubscription fast)
          slow2 <- atomically (takeSubscription slow)
          pure { fast: fast2, slow: slowFirst + slow2 }
      result <- runRIO' program
      result `shouldEqual` { fast: 2, slow: 1 + 2 }
      order <- liftEffect (Ref.read events)
      order `shouldEqual`
        [ "fast-drained"
        , "before-slow-drain"
        , "publish-2-committed"
        , "after-join"
        ]

    it "with no subscribers, publish never blocks (no buffer to fill)" do
      let
        program :: RIO () () Boolean
        program = do
          (hub :: THub Int) <- atomically (newBoundedTHub 0)
          atomically (publishTHub hub 1)
      result <- runRIO' program
      result `shouldEqual` true

  describe "Unbounded strategy" do
    it "never drops; many publishes buffer up and drain in order" do
      let
        n = 50

        program :: RIO () () { len :: Int, drained :: Array Int }
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          traverse_ (\k -> atomically (publishTHub hub k) >>= \_ -> pure unit)
            (range 1 n)
          len <- atomically (lengthSubscription sub)
          drained <- traverse (\_ -> atomically (takeSubscription sub))
            (range 1 n)
          pure { len, drained }
      result <- runRIO' program
      result.len `shouldEqual` n
      result.drained `shouldEqual` range 1 n

  describe "unsubscribe" do
    it "after unsubscribe, further publishes are not delivered" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (takeSubscription sub)
          atomically (unsubscribeTHub sub)
          _ <- atomically (publishTHub hub 2)
          atomically (tryTakeSubscription sub)
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "withSubscription" do
    it "releases the subscription on success" do
      let
        program :: RIO () () { during :: Int, after :: Int }
        program = do
          hub <- atomically newUnboundedTHub
          during <- withSubscription hub \sub -> do
            _ <- atomically (publishTHub hub 7)
            v <- atomically (takeSubscription sub)
            n <- atomically (subscriberCount hub)
            pure (v + n)
          after <- atomically (subscriberCount hub)
          pure { during, after }
      result <- runRIO' program
      result `shouldEqual` { during: 8, after: 0 }

    it "releases the subscription on a typed failure inside the body" do
      -- Docstring promise: "released on every termination path of `use`
      -- (success, typed failure, defect, interrupt)". The success path
      -- is covered above; this pins the typed-failure path: build a
      -- hub, subscribe via withSubscription, raise a typed failure
      -- from the body, and observe that subscriberCount drops back
      -- to zero after the failure surfaces.
      hubRef <- liftEffect (Ref.new Nothing)
      let
        program :: RIO () (boom :: Unit) Unit
        program = do
          hub <- atomically (newUnboundedTHub :: _ (THub Int))
          liftEffect (Ref.write (Just hub) hubRef)
          withSubscription hub \_ ->
            fail (Proxy :: Proxy "boom") unit
      result <- runRIO program
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
      maybeHub <- liftEffect (Ref.read hubRef)
      case maybeHub of
        Nothing -> 1 `shouldEqual` 0
        Just hub -> do
          after <- runRIO' (atomically (subscriberCount hub))
          after `shouldEqual` 0

    it "releases the subscription on a defect inside the body" do
      -- Same docstring contract; pin the defect path so the full
      -- bracket is documented across all four termination paths.
      hubRef <- liftEffect (Ref.new Nothing)
      let
        program :: RIO () () Unit
        program = do
          hub <- atomically (newUnboundedTHub :: _ (THub Int))
          liftEffect (Ref.write (Just hub) hubRef)
          withSubscription hub \_ ->
            die (error "kaboom")
      _ <- attempt (runRIO' program)
      maybeHub <- liftEffect (Ref.read hubRef)
      case maybeHub of
        Nothing -> 1 `shouldEqual` 0
        Just hub -> do
          after <- runRIO' (atomically (subscriberCount hub))
          after `shouldEqual` 0

    it "releases the subscription on a fiber kill inside the body" do
      -- Pin the fourth and last termination path. Killing the
      -- fiber mid-body must still trigger the bracket-based
      -- unsubscribe.
      hubRef <- liftEffect (Ref.new Nothing)
      let
        program :: RIO () () Unit
        program = do
          hub <- atomically (newUnboundedTHub :: _ (THub Int))
          liftEffect (Ref.write (Just hub) hubRef)
          withSubscription hub \_ ->
            liftAff (delay (Milliseconds 50.0))
      f <- forkAff (runRIO' program)
      delay (Milliseconds 5.0)
      killFiber (error "test-cancel") f
      delay (Milliseconds 10.0)
      maybeHub <- liftEffect (Ref.read hubRef)
      case maybeHub of
        Nothing -> 1 `shouldEqual` 0
        Just hub -> do
          after <- runRIO' (atomically (subscriberCount hub))
          after `shouldEqual` 0

  describe "isEmptySubscription" do
    it "returns true on a fresh subscription with no published values" do
      let
        program :: RIO () () Boolean
        program = do
          hub <- atomically (newUnboundedTHub :: _ (THub Int))
          sub <- atomically (subscribeTHub hub)
          atomically (isEmptySubscription sub)
      result <- runRIO' program
      result `shouldEqual` true

    it "returns false while values are buffered, true again after drain" do
      let
        program :: RIO () () { afterPublish :: Boolean, afterDrain :: Boolean }
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (publishTHub hub 2)
          afterPublish <- atomically (isEmptySubscription sub)
          _ <- atomically (takeSubscription sub)
          _ <- atomically (takeSubscription sub)
          afterDrain <- atomically (isEmptySubscription sub)
          pure { afterPublish, afterDrain }
      result <- runRIO' program
      result `shouldEqual` { afterPublish: false, afterDrain: true }
