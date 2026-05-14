module Test.RIO.Stream.ParSpec (spec) where

import Prelude hiding (join)

import Data.Array (filter, length, range, sort) as Array
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (attempt, delay, error)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Concurrency (fork, join)
import RIO.Core (RIO, die, fail, runRIO, runRIO')
import RIO.Stream (Stream, fromArray, mapM, runCollect)
import RIO.Stream.Par (broadcast, merge, mergeAll, mergeMap, partition)

spec :: Spec Unit
spec = do
  describe "RIO.Stream.Par" do

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
