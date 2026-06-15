module Test.RIO.Aff.Stream.ParSpec (spec) where

import Prelude hiding (join)

import Data.Array (filter, index, length, range, sort) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect.Aff (attempt, delay, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (fork, join)
import RIO.Aff.Core (RIO, die, fail, runRIO, runRIO')
import RIO.Aff.Stream (Stream, fromArray, mapM, runCollect)
import RIO.Aff.Stream.Par
  ( broadcast
  , merge
  , mergeAll
  , mergeMap
  , partition
  , partitionEither
  )

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Stream.Par" do

    describe "mergeAll" do
      it "produces an empty stream from an empty input array" do
        r <- runRIO
          ( runCollect (mergeAll [] :: Stream () () Int)
          )
        r `shouldEqual` Right []

      it "yields every value across all inputs" do
        let
          s1 = fromArray [ 1, 2, 3 ]
          s2 = fromArray [ 10, 20, 30 ]
          s3 = fromArray [ 100, 200, 300 ]
        r <- runRIO (runCollect (mergeAll [ s1, s2, s3 ]))
        case r of
          Right xs ->
            Array.sort xs
              `shouldEqual` [ 1, 2, 3, 10, 20, 30, 100, 200, 300 ]
          Left _ ->
            Spec.fail "expected mergeAll to succeed"

      it "single-stream input behaves like the input" do
        r <- runRIO
          (runCollect (mergeAll [ fromArray (Array.range 1 5) ]))
        case r of
          Right xs -> Array.sort xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
          Left _ -> Spec.fail "expected single-stream mergeAll to succeed"

      it "preserves each input's internal order in the merged output" do
        -- Docstring promise: "Output order is non-deterministic
        -- across inputs but preserves each input's internal
        -- order." Existing tests sort the output, so a regression
        -- that scrambled per-input order would not be caught.
        -- Pin the contract by filtering the merged output back
        -- into each input's slice and checking each is in order.
        let
          s1 = fromArray [ 1, 2, 3, 4, 5 ]
          s2 = fromArray [ 100, 200, 300, 400, 500 ]
        r <- runRIO (runCollect (mergeAll [ s1, s2 ]))
        case r of
          Right xs -> do
            Array.filter (_ < 100) xs `shouldEqual`
              [ 1, 2, 3, 4, 5 ]
            Array.filter (_ >= 100) xs `shouldEqual`
              [ 100, 200, 300, 400, 500 ]
          Left _ -> Spec.fail "expected mergeAll to succeed"

      it "interleaves slow producers (both contribute)" do
        let
          slow ms n = mapM
            (\v -> liftAff (delay (Milliseconds ms)) *> pure v)
            (fromArray [ n, n + 1, n + 2 ])
          s1 = slow 5.0 1
          s2 = slow 5.0 100
        r <- runRIO (runCollect (mergeAll [ s1, s2 ]))
        case r of
          Right xs -> do
            Array.sort xs `shouldEqual` [ 1, 2, 3, 100, 101, 102 ]
          Left _ -> Spec.fail "expected mergeAll to succeed"

      it "propagates a typed failure from one producer" do
        let
          bad :: Stream () (boom :: String) Int
          bad = mapM
            (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
            (fromArray [ 1 ])

          good :: Stream () (boom :: String) Int
          good = fromArray [ 2, 3 ]
        r <- runRIO (runCollect (mergeAll [ good, bad ]))
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected mergeAll to surface the typed failure"

      it "propagates a defect from one producer" do
        -- Module docstring promises a single failure model for
        -- every combinator here: "the first typed failure or
        -- defect observed in any producer shuts the shared queue
        -- down". Typed failure is pinned above; pin the defect
        -- path so the full contract is documented. A defect
        -- raised by `die` inside a producer must surface as an
        -- `Aff` exception on the consumer's pull, observable via
        -- `attempt`.
        let
          bad :: Stream () () Int
          bad = mapM
            (\_ -> die (error "kaboom"))
            (fromArray [ 1 ])

          good :: Stream () () Int
          good = fromArray [ 2, 3 ]
        r <- attempt
          (runRIO' (runCollect (mergeAll [ good, bad ])))
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected mergeAll to surface the defect"

    describe "merge" do
      it "two-stream merge collects every element" do
        r <- runRIO
          ( runCollect
              ( merge (fromArray [ 1, 2, 3 ])
                  (fromArray [ 4, 5, 6 ])
              )
          )
        case r of
          Right xs ->
            Array.sort xs `shouldEqual` [ 1, 2, 3, 4, 5, 6 ]
          Left _ -> Spec.fail "expected merge to succeed"

    describe "mergeMap" do
      it "fans each outer element out into its inner stream" do
        let
          outer = fromArray [ 1, 2, 3 ]
          f n = fromArray [ n, n * 10 ]
        r <- runRIO (runCollect (mergeMap f outer))
        case r of
          Right xs ->
            Array.sort xs `shouldEqual` [ 1, 2, 3, 10, 20, 30 ]
          Left _ -> Spec.fail "expected mergeMap to succeed"

      it "empty outer produces empty output" do
        let
          program :: RIO () () (Array Int)
          program = runCollect
            (mergeMap (\n -> fromArray [ n ]) (fromArray []))
        r <- runRIO program
        r `shouldEqual` Right []

      it "empty inners contribute nothing" do
        let
          program :: RIO () () (Array Int)
          program = runCollect
            ( mergeMap (\_ -> fromArray ([] :: Array Int))
                (fromArray [ 1, 2, 3 ])
            )
        r <- runRIO program
        r `shouldEqual` Right []

      it "propagates a typed failure raised inside an inner stream" do
        -- The module-level docstring promises all combinators share
        -- one failure model: the first typed failure or defect in any
        -- producer shuts the shared queue down. `mergeAll` has a
        -- dedicated test; this pins the same contract for `mergeMap`,
        -- whose producers are the per-element inner streams.
        let
          inner :: Int -> Stream () (boom :: String) Int
          inner n
            | n == 2 =
                mapM (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
                  (fromArray [ n ])
            | otherwise = fromArray [ n ]

          outer :: Stream () (boom :: String) Int
          outer = fromArray [ 1, 2, 3 ]
        r <- runRIO (runCollect (mergeMap inner outer))
        case r of
          Left _ -> pure unit
          Right _ ->
            Spec.fail "expected mergeMap to surface the inner typed failure"

    describe "broadcast" do
      it "every consumer sees every element in input order" do
        let
          program :: RIO () () (Array (Array Int))
          program = do
            consumers <- broadcast 3 4 (fromArray (Array.range 1 5))
            fibers <- traverse (\s -> fork (runCollect s)) consumers
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs -> do
            outs `shouldEqual`
              [ [ 1, 2, 3, 4, 5 ]
              , [ 1, 2, 3, 4, 5 ]
              , [ 1, 2, 3, 4, 5 ]
              ]
          Left _ -> Spec.fail "expected broadcast to succeed"

      it "n = 0 returns no consumers (no fiber forked)" do
        let
          program :: RIO () () (Array (Stream () () Int))
          program = broadcast 0 4 (fromArray [ 1, 2, 3 ])
        r <- runRIO program
        case r of
          Right xs -> Array.length xs `shouldEqual` 0
          Left _ -> Spec.fail "expected zero-consumer broadcast to succeed"

      it "n = 0 does not touch the upstream (no upstream effects ever run)" do
        -- Docstring promise: "`n <= 0` returns an empty array
        -- immediately and does not touch the upstream." The
        -- pinned `n = 0` test uses a pure `fromArray` upstream
        -- and only checks the consumer-array length, so it
        -- can't observe whether upstream effects were run. A
        -- regression that always forked the producer (even for
        -- n = 0) would still pass that test: the consumer array
        -- would still be empty because the producer's queue
        -- list is empty. Pin the "does not touch upstream" half
        -- with an effectful counting upstream: any pull from
        -- inside a producer fiber would increment the counter.
        -- After running broadcast and giving any rogue producer
        -- fiber time to start, the counter must still be zero.
        pulls <- liftEffect (Ref.new 0)
        let
          counted :: Stream () () Int
          counted = mapM
            ( \n -> do
                _ <- liftEffect (Ref.modify (_ + 1) pulls)
                pure n
            )
            (fromArray [ 1, 2, 3 ])

          program :: RIO () () (Array (Stream () () Int))
          program = broadcast 0 4 counted
        _ <- runRIO program
        -- Give any incorrectly-forked producer fiber a chance to
        -- start pulling before we check.
        liftAff (delay (Milliseconds 20.0))
        n <- liftEffect (Ref.read pulls)
        n `shouldEqual` 0

      it "propagates a typed failure to every consumer" do
        let
          source :: Stream () (boom :: String) Int
          source = mapM
            (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
            (fromArray [ 1 ])

          program :: RIO () (boom :: String) (Array (Array Int))
          program = do
            consumers <- broadcast 2 4 source
            fibers <- traverse (\s -> fork (runCollect s)) consumers
            traverse join fibers
        r <- runRIO program
        case r of
          Left _ -> pure unit
          Right _ ->
            Spec.fail "expected broadcast to surface the typed failure"

      it "bufferSize <= 0 is clamped to at least 1 (no crash, all elements delivered)" do
        -- The docstring promises "`bufferSize` is clamped to
        -- at least 1." Every other `broadcast` test passes a
        -- positive bufferSize (4 or 2), so a regression that
        -- removed the `let cap = max 1 bufferSize` clamp and
        -- forwarded the raw size to `Queue.bounded` would
        -- silently break only the zero/negative path: bounded
        -- queue capacity of 0 would block the producer's first
        -- offer forever (no consumer slot to fill). Pin the
        -- clamp with `bufferSize = 0`: with the clamp,
        -- every consumer must still observe the full input
        -- stream.
        let
          program :: RIO () () (Array (Array Int))
          program = do
            consumers <- broadcast 2 0 (fromArray [ 1, 2, 3 ])
            fibers <- traverse (\s -> fork (runCollect s)) consumers
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual` [ [ 1, 2, 3 ], [ 1, 2, 3 ] ]
          Left _ -> Spec.fail "expected clamped-bufferSize broadcast to succeed"

      it "propagates a defect through a consumer's pull" do
        -- Docstring promise: "the first observed typed failure
        -- or defect on the producer side shuts every subscriber
        -- queue down". The typed-failure half is pinned above;
        -- pin the defect half so both branches of
        -- `propagateCause` (`Fail` and `Die`) are documented
        -- through the `broadcast` surface. `broadcast` has its
        -- own producer (`broadcastProducer`) distinct from
        -- `mergeAll`'s `produce`, so a regression that swapped
        -- `attemptCause` for `attempt` in `broadcastProducer`
        -- (or otherwise mis-handled the `Die` case) would still
        -- pass `mergeAll`'s defect test and the
        -- `broadcast`/typed-failure test but break here. A
        -- `die` inside the producer must surface as an `Aff`
        -- exception on the consumer's pull, observable through
        -- `attempt`. Only one consumer is run here: a second
        -- consumer that also propagates the defect on its pull
        -- would leave an orphan fiber whose error escapes to
        -- the runtime top level. The one-consumer form is
        -- sufficient to exercise `broadcastProducer`'s defect
        -- path.
        let
          source :: Stream () () Int
          source = mapM
            (\_ -> die (error "kaboom"))
            (fromArray [ 1 ])

          program :: RIO () () (Array Int)
          program = do
            consumers <- broadcast 2 4 source
            case Array.index consumers 0 of
              Nothing -> pure []
              Just s -> runCollect s
        r <- attempt (runRIO' program)
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail
            "expected broadcast to surface the defect on the consumer pull"

    describe "partition" do
      it "routes each element to exactly one bucket (even/odd)" do
        let
          program :: RIO () () (Array (Array Int))
          program = do
            buckets <- partition 2 4 identity
              (fromArray (Array.range 1 6))
            fibers <- traverse (\s -> fork (runCollect s)) buckets
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual` [ [ 2, 4, 6 ], [ 1, 3, 5 ] ]
          Left _ -> Spec.fail "expected partition to succeed"

      it "preserves per-bucket input order" do
        let
          program :: RIO () () (Array (Array Int))
          program = do
            buckets <- partition 3 4 (\n -> n `mod` 3)
              (fromArray (Array.range 1 9))
            fibers <- traverse (\s -> fork (runCollect s)) buckets
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual`
              [ [ 3, 6, 9 ], [ 1, 4, 7 ], [ 2, 5, 8 ] ]
          Left _ -> Spec.fail "expected partition to succeed"

      it "handles negative keys (mod normalises)" do
        let
          program :: RIO () () (Array (Array Int))
          program = do
            buckets <- partition 2 4 negate
              (fromArray [ 1, 2, 3, 4 ])
            fibers <- traverse (\s -> fork (runCollect s)) buckets
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual` [ [ 2, 4 ], [ 1, 3 ] ]
          Left _ -> Spec.fail "expected partition to succeed"

      it "n = 0 returns no buckets" do
        let
          program :: RIO () () (Array (Stream () () Int))
          program = partition 0 4 identity (fromArray [ 1, 2, 3 ])
        r <- runRIO program
        case r of
          Right xs -> Array.length xs `shouldEqual` 0
          Left _ -> Spec.fail "expected zero-bucket partition to succeed"

      it "n = 0 does not touch the upstream (no upstream effects ever run)" do
        -- Docstring promise: "`n <= 0` returns an empty array
        -- immediately and does not touch the upstream." The
        -- pinned `n = 0 returns no buckets` test uses a pure
        -- `fromArray` upstream and only checks the bucket-array
        -- length, so it can't observe whether upstream effects
        -- were run. A regression that always forked the producer
        -- (even for n = 0) would still pass that test: the
        -- bucket array would still be empty because there are
        -- no buckets to route to. Pin the "does not touch
        -- upstream" half with an effectful counting upstream:
        -- any pull from inside a producer fiber would increment
        -- the counter. After running partition and giving any
        -- rogue producer fiber time to start, the counter must
        -- still be zero. Symmetric to the broadcast n = 0 test.
        pulls <- liftEffect (Ref.new 0)
        let
          counted :: Stream () () Int
          counted = mapM
            ( \n -> do
                _ <- liftEffect (Ref.modify (_ + 1) pulls)
                pure n
            )
            (fromArray [ 1, 2, 3 ])

          program :: RIO () () (Array (Stream () () Int))
          program = partition 0 4 identity counted
        _ <- runRIO program
        -- Give any incorrectly-forked producer fiber a chance to
        -- start pulling before we check.
        liftAff (delay (Milliseconds 20.0))
        n <- liftEffect (Ref.read pulls)
        n `shouldEqual` 0

      it "propagates a typed failure to every bucket" do
        let
          source :: Stream () (boom :: String) Int
          source = mapM
            (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
            (fromArray [ 1 ])

          program :: RIO () (boom :: String) (Array (Array Int))
          program = do
            buckets <- partition 2 4 identity source
            fibers <- traverse (\s -> fork (runCollect s)) buckets
            traverse join fibers
        r <- runRIO program
        case r of
          Left _ -> pure unit
          Right _ ->
            Spec.fail "expected partition to surface the typed failure"

      it "propagates a defect through a bucket's pull" do
        -- Docstring promise (parallel to `broadcast`'s
        -- defect pin): "the first observed producer-side
        -- failure or defect shuts every bucket down, and
        -- every consumer surfaces the same captured cause on
        -- its next pull." The typed-failure half is pinned
        -- above; `partition`'s producer
        -- (`partitionProducer`) is a separate function from
        -- `broadcastProducer` and `produce`, so a regression
        -- that swapped `attemptCause` for `attempt` in
        -- `partitionProducer` (or otherwise mis-handled the
        -- `Die` case) would still pass the other defect tests
        -- but break here. Only one bucket is run for the
        -- same reason as `broadcast`'s defect pin: a second
        -- consumer that also propagates the defect would
        -- leave an orphan fiber whose error escapes to the
        -- runtime top level.
        let
          source :: Stream () () Int
          source = mapM
            (\_ -> die (error "kaboom"))
            (fromArray [ 1 ])

          program :: RIO () () (Array Int)
          program = do
            buckets <- partition 2 4 identity source
            case Array.index buckets 0 of
              Nothing -> pure []
              Just s -> runCollect s
        r <- attempt (runRIO' program)
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail
            "expected partition to surface the defect on the bucket pull"

      it "bufferSize <= 0 is clamped to at least 1 (no crash, all elements routed)" do
        -- The docstring promises "Each bucket has its own
        -- bounded queue of size `bufferSize` (clamped to at
        -- least 1)". The parallel claim for `broadcast` is
        -- pinned above; `partition` makes the identical
        -- promise via `let cap = max 1 bufferSize` in
        -- `Stream/Par.purs` and is otherwise untested:
        -- every existing `partition` test passes bufferSize 4.
        -- A regression that dropped the clamp and forwarded a
        -- raw 0 to `Queue.bounded` would deadlock the
        -- producer's first offer (zero-capacity queue). Pin
        -- the clamp by partitioning into 2 buckets with
        -- `bufferSize = 0` and asserting every element is
        -- routed (bucket 0 gets even-residue elements, bucket
        -- 1 gets odd-residue).
        let
          program :: RIO () () (Array (Array Int))
          program = do
            buckets <- partition 2 0 identity (fromArray [ 1, 2, 3, 4 ])
            fibers <- traverse (\s -> fork (runCollect s)) buckets
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual` [ [ 2, 4 ], [ 1, 3 ] ]
          Left _ -> Spec.fail "expected clamped-bufferSize partition to succeed"

    describe "partitionEither" do
      it "routes every Left to lefts and every Right to rights" do
        let
          input :: Stream () () (Either Int String)
          input = fromArray
            [ Left 1, Right "a", Left 2, Right "b", Left 3, Right "c" ]

          program :: RIO () () (Tuple (Array Int) (Array String))
          program = do
            { lefts, rights } <- partitionEither input
            lf <- fork (runCollect lefts)
            rf <- fork (runCollect rights)
            ls <- join lf
            rs <- join rf
            pure (Tuple ls rs)
        r <- runRIO program
        case r of
          Right (Tuple ls rs) -> do
            ls `shouldEqual` [ 1, 2, 3 ]
            rs `shouldEqual` [ "a", "b", "c" ]
          Left _ -> Spec.fail "expected partitionEither to succeed"

      it "empty input closes both consumer streams" do
        let
          input :: Stream () () (Either Int String)
          input = fromArray []

          program :: RIO () () (Tuple (Array Int) (Array String))
          program = do
            { lefts, rights } <- partitionEither input
            lf <- fork (runCollect lefts)
            rf <- fork (runCollect rights)
            ls <- join lf
            rs <- join rf
            pure (Tuple ls rs)
        r <- runRIO program
        case r of
          Right (Tuple ls rs) -> do
            ls `shouldEqual` ([] :: Array Int)
            rs `shouldEqual` ([] :: Array String)
          Left _ -> Spec.fail "expected empty partitionEither to succeed"

      it "all-Left input drains lefts and closes rights" do
        let
          input :: Stream () () (Either Int String)
          input = fromArray [ Left 7, Left 8, Left 9 ]

          program :: RIO () () (Tuple (Array Int) (Array String))
          program = do
            { lefts, rights } <- partitionEither input
            lf <- fork (runCollect lefts)
            rf <- fork (runCollect rights)
            ls <- join lf
            rs <- join rf
            pure (Tuple ls rs)
        r <- runRIO program
        case r of
          Right (Tuple ls rs) -> do
            ls `shouldEqual` [ 7, 8, 9 ]
            rs `shouldEqual` ([] :: Array String)
          Left _ -> Spec.fail "expected all-Left partitionEither to succeed"

      it "propagates a typed failure raised by the upstream" do
        let
          producer :: Stream () (boom :: Int) (Either Int String)
          producer = mapM
            ( \i ->
                if i == 2 then fail (Proxy :: Proxy "boom") i
                else pure (Left i)
            )
            (fromArray [ 1, 2, 3 ])

          program :: RIO () (boom :: Int) (Array Int)
          program = do
            { lefts } <- partitionEither producer
            runCollect lefts
        r <- runRIO program
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected partitionEither failure to surface"

    describe "backpressure timing" do
      it "slow consumers don't lose elements (buffer + backpressure)" do
        let
          program :: RIO () () (Array (Array Int))
          program = do
            consumers <- broadcast 2 2 (fromArray (Array.range 1 6))
            fibers <- traverse
              ( \s -> fork
                  ( runCollect
                      ( mapM
                          ( \v ->
                              liftAff (delay (Milliseconds 2.0))
                                *> pure v
                          )
                          s
                      )
                  )
              )
              consumers
            traverse join fibers
        r <- runRIO program
        case r of
          Right outs ->
            outs `shouldEqual`
              [ [ 1, 2, 3, 4, 5, 6 ]
              , [ 1, 2, 3, 4, 5, 6 ]
              ]
          Left _ -> Spec.fail "expected backpressured broadcast to succeed"
