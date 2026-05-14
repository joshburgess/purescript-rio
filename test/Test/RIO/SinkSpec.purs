module Test.RIO.SinkSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO, runRIO')
import RIO.Sink (Sink)
import RIO.Sink as Sink
import RIO.Stream (Stream)
import RIO.Stream as Stream

source :: Stream () () Int
source = Stream.fromArray [ 1, 2, 3, 4, 5 ]

spec :: Spec Unit
spec = describe "RIO.Sink" do

  describe "primitives" do
    it "drain returns unit and consumes the whole stream" do
      r <- runRIO' (Sink.runSink Sink.drain source)
      r `shouldEqual` unit

    it "head returns Just of the first element" do
      r <- runRIO' (Sink.runSink Sink.head source)
      r `shouldEqual` Just 1

    it "head returns Nothing on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.head :: Sink () () Int (Maybe Int)) Stream.empty)
      r `shouldEqual` Nothing

    it "last returns Just of the final element" do
      r <- runRIO' (Sink.runSink Sink.last source)
      r `shouldEqual` Just 5

    it "last returns Nothing on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.last :: Sink () () Int (Maybe Int)) Stream.empty)
      r `shouldEqual` Nothing

    it "count returns the number of elements" do
      r <- runRIO' (Sink.runSink Sink.count source)
      r `shouldEqual` 5

    it "count returns 0 on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.count :: Sink () () Int Int) Stream.empty)
      r `shouldEqual` 0

    it "collect returns every element in input order" do
      r <- runRIO' (Sink.runSink Sink.collect source)
      r `shouldEqual` [ 1, 2, 3, 4, 5 ]

  describe "folds" do
    it "foldL accumulates with a pure step" do
      r <- runRIO' (Sink.runSink (Sink.foldL 0 (+)) source)
      r `shouldEqual` 15

    it "foldM accumulates with an effectful step" do
      ref <- liftEffect (Ref.new 0)
      let
        step acc i = do
          liftEffect (Ref.modify_ (_ + 1) ref)
          pure (acc + i)
      r <- runRIO' (Sink.runSink (Sink.foldM 0 step) source)
      r `shouldEqual` 15
      calls <- liftEffect (Ref.read ref)
      calls `shouldEqual` 5

    it "foldM typed failure surfaces and halts consumption" do
      -- The whole Sink suite had no failure-path coverage. Pin the
      -- contract that a typed failure raised inside a foldM step
      -- propagates on the parent's row and stops further input from
      -- being consumed (here observed via a Ref-based call counter:
      -- the step ran on elements 1, 2, 3 and then failed on 3 before
      -- the rest of the stream was reached).
      ref <- liftEffect (Ref.new 0)
      let
        step :: Int -> Int -> RIO () (boom :: Int) Int
        step acc i = do
          liftEffect (Ref.modify_ (_ + 1) ref)
          if i == 3 then fail (Proxy :: Proxy "boom") i
          else pure (acc + i)

        program :: RIO () (boom :: Int) Int
        program = Sink.runSink (Sink.foldM 0 step)
          (Stream.fromArray [ 1, 2, 3, 4, 5 ])
      result <- runRIO program
      calls <- liftEffect (Ref.read ref)
      case result of
        Left _ -> pure unit
        Right _ -> Spec.fail "expected foldM step failure to surface"
      calls `shouldEqual` 3

  describe "short-circuiting" do
    it "take n returns the first n elements" do
      r <- runRIO' (Sink.runSink (Sink.take 3) source)
      r `shouldEqual` [ 1, 2, 3 ]

    it "take 0 returns the empty array without pulling" do
      r <- runRIO' (Sink.runSink (Sink.take 0) source)
      r `shouldEqual` ([] :: Array Int)

    it "take 0 does not pull any element from the source" do
      -- Docstring + implementation guard `take n | n <= 0 =
      -- Sink (pure (Halt []))` promise that `take 0` halts
      -- before pulling. The pinned "take 0 returns the empty
      -- array without pulling" test only checks the result
      -- equals `[]`. A regression that weakened the guard to
      -- `n < 0` would still pass that test: the `go [] 0`
      -- branch immediately returns `Halt acc` without ever
      -- pulling either. Pin the no-pull half with a counting
      -- source via `repeatM`: every pull from an infinite
      -- counting source would increment the counter, so the
      -- counter staying at 0 proves the guard short-circuits
      -- before any pull.
      counter <- liftEffect (Ref.new 0)
      let
        tick :: RIO () () Int
        tick = liftEffect (Ref.modify (_ + 1) counter)

        program :: RIO () () (Array Int)
        program = Sink.runSink (Sink.take 0) (Stream.repeatM tick)
      r <- runRIO' program
      r `shouldEqual` ([] :: Array Int)
      pulls <- liftEffect (Ref.read counter)
      pulls `shouldEqual` 0

    it "take returns what it could when the stream is short" do
      r <- runRIO' (Sink.runSink (Sink.take 10) source)
      r `shouldEqual` [ 1, 2, 3, 4, 5 ]

    it "find returns the first match and halts" do
      r <- runRIO' (Sink.runSink (Sink.find (_ > 3)) source)
      r `shouldEqual` Just 4

    it "find does not pull elements after the first match" do
      -- The pinned "find returns the first match and halts"
      -- test names the halt half but only asserts the result
      -- equals `Just 4`. A regression that kept scanning after
      -- the match while remembering the first match (e.g.
      -- `Need (\_ -> ...) (pure (Just i))` in place of
      -- `Halt (Just i)`) would still return `Just 4` and pass
      -- that test. Symmetric to the just-pinned `any` and `all`
      -- short-circuit tests: assert that exactly the prefix up
      -- to and including the matching element is pulled.
      counter <- liftEffect (Ref.new 0)
      let
        tick :: Int -> RIO () () Int
        tick i = liftEffect (Ref.modify (_ + 1) counter) *> pure i

        program :: RIO () () (Maybe Int)
        program = Sink.runSink
          (Sink.find (_ > 3))
          (Stream.mapM tick (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
      r <- runRIO' program
      r `shouldEqual` Just 4
      pulled <- liftEffect (Ref.read counter)
      pulled `shouldEqual` 4

    it "find returns Nothing when nothing matches" do
      r <- runRIO' (Sink.runSink (Sink.find (_ > 100)) source)
      r `shouldEqual` Nothing

    it "any short-circuits on a match" do
      r <- runRIO' (Sink.runSink (Sink.any (_ == 3)) source)
      r `shouldEqual` true

    it "any does not pull elements after the first match" do
      -- Docstring promise: "Short-circuits on the first match."
      -- The pinned "any short-circuits on a match" test only
      -- asserts the return value equals `true`, which any
      -- implementation that scans the full stream would also
      -- satisfy. A regression that replaced `Halt true` with
      -- `any p` in the match branch (i.e. kept consuming after
      -- the match) would still return `true` and pass that
      -- test. Pin the short-circuit half with a counting
      -- upstream and assert exactly the prefix up to and
      -- including the matching element was pulled.
      counter <- liftEffect (Ref.new 0)
      let
        tick :: Int -> RIO () () Int
        tick i = liftEffect (Ref.modify (_ + 1) counter) *> pure i

        program :: RIO () () Boolean
        program = Sink.runSink
          (Sink.any (_ == 3))
          (Stream.mapM tick (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
      r <- runRIO' program
      r `shouldEqual` true
      pulled <- liftEffect (Ref.read counter)
      pulled `shouldEqual` 3

    it "any returns false on an empty stream" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.any (_ == 3) :: Sink () () Int Boolean)
            Stream.empty
        )
      r `shouldEqual` false

    it "all short-circuits on a non-match" do
      r <- runRIO' (Sink.runSink (Sink.all (_ < 4)) source)
      r `shouldEqual` false

    it "all does not pull elements after the first non-match" do
      -- Symmetric to the `any` short-circuit pin. Docstring
      -- promise: `all` "Short-circuits on the first non-match."
      -- The pinned "all short-circuits on a non-match" test
      -- only asserts the return value equals `false`, which any
      -- implementation that scans the full stream would satisfy.
      -- A regression that replaced `Halt false` with `all p` in
      -- the non-match branch (kept consuming after the failure)
      -- would still return `false` and pass that test. Pin the
      -- short-circuit half with a counting upstream and assert
      -- exactly the prefix up to and including the first
      -- non-matching element was pulled.
      counter <- liftEffect (Ref.new 0)
      let
        tick :: Int -> RIO () () Int
        tick i = liftEffect (Ref.modify (_ + 1) counter) *> pure i

        program :: RIO () () Boolean
        program = Sink.runSink
          (Sink.all (_ < 4))
          (Stream.mapM tick (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
      r <- runRIO' program
      r `shouldEqual` false
      pulled <- liftEffect (Ref.read counter)
      pulled `shouldEqual` 4

    it "all returns true on an empty stream" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.all (_ < 4) :: Sink () () Int Boolean)
            Stream.empty
        )
      r `shouldEqual` true

  describe "combinators" do
    it "mapResult post-processes the result" do
      r <- runRIO' (Sink.runSink (Sink.mapResult show Sink.count) source)
      r `shouldEqual` "5"

    it "mapInput pre-processes each element" do
      r <- runRIO' (Sink.runSink (Sink.mapInput show Sink.collect) source)
      r `shouldEqual` [ "1", "2", "3", "4", "5" ]

    it "filterIn drops elements before the inner sink sees them" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.filterIn (\n -> n `mod` 2 == 0) Sink.collect)
            source
        )
      r `shouldEqual` [ 2, 4 ]

    it "filterIn: filtered-out elements do not count against an inner take's budget" do
      -- The `filterIn` docstring promises that inputs failing
      -- the predicate are dropped "before feeding them to the
      -- underlying sink." The existing test pairs `filterIn`
      -- with `collect`, which never halts early and has no
      -- input-count budget, so it can't observe whether a
      -- predicate-failing input is silently forwarded to the
      -- inner sink's `k`. A regression that called `k i` for
      -- both branches of the predicate (instead of recycling
      -- the wrapped sink) would have failing inputs eating
      -- the budget of a counting inner sink like `take 2`,
      -- and `filterIn even (take 2)` would halt early on the
      -- first odd input rather than wait for two evens. Pin
      -- the recycle-not-forward behaviour with `take 2` over
      -- `[1, 2, 3, 4, 5]`: only `[2, 4]` may be collected,
      -- and `5` is never pulled.
      r <- runRIO'
        ( Sink.runSink
            (Sink.filterIn (\n -> n `mod` 2 == 0) (Sink.take 2))
            source
        )
      r `shouldEqual` [ 2, 4 ]

    it "andThen sequences two sinks at the same stream position" do
      let
        sink = Sink.head `Sink.andThen` \mFirst ->
          Sink.mapResult (\rest -> { first: mFirst, rest }) Sink.collect
      r <- runRIO' (Sink.runSink sink source)
      r `shouldEqual` { first: Just 1, rest: [ 2, 3, 4, 5 ] }

    it "andThen on empty stream runs the second sink's finish" do
      let
        sink = Sink.head `Sink.andThen` \_ -> Sink.count
      r <- runRIO' (Sink.runSink (sink :: Sink () () Int Int) Stream.empty)
      r `shouldEqual` 0

    it "andThen feeds the first sink's finish value into k when the stream ends mid-consumption" do
      -- Docstring promise: "If the stream ends while the first
      -- sink is still consuming, the first sink's `finish` runs,
      -- the result is fed into `k`, and the resulting second
      -- sink is run against an empty stream (so it returns its
      -- own `finish` value)." The existing empty-stream test
      -- discards the first result with `\_`, so it only pins
      -- the third clause. Pin all three by using `take 10`
      -- against a 5-element stream: the first sink must
      -- accumulate [1..5], its `finish` must produce that array,
      -- the closure must receive it, and the second sink (here
      -- `collect`) must run on the empty remainder.
      let
        sink = Sink.take 10 `Sink.andThen` \first ->
          Sink.mapResult (\rest -> { first, rest }) Sink.collect
      r <- runRIO' (Sink.runSink sink source)
      r `shouldEqual`
        { first: [ 1, 2, 3, 4, 5 ], rest: [] :: Array Int }

  describe "zipPar" do
    it "runs two sinks against the same stream and tuples the results" do
      r <- runRIO' (Sink.runSink (Sink.zipPar Sink.count Sink.collect) source)
      r `shouldEqual` Tuple 5 [ 1, 2, 3, 4, 5 ]

    it "one side halts early; the other keeps consuming" do
      -- take 2 halts after seeing 1 and 2; count keeps going through end
      r <- runRIO'
        (Sink.runSink (Sink.zipPar (Sink.take 2) Sink.count) source)
      r `shouldEqual` Tuple [ 1, 2 ] 5

    it "both sides halt early; the combined sink halts immediately after" do
      -- take 2 and take 3 both halt; combined halts at max(2,3) = 3
      r <- runRIO'
        (Sink.runSink (Sink.zipPar (Sink.take 2) (Sink.take 3)) source)
      r `shouldEqual` Tuple [ 1, 2 ] [ 1, 2, 3 ]

    it "on an empty stream both sides return their finish values" do
      r <- runRIO'
        ( Sink.runSink
            ( Sink.zipPar
                (Sink.count :: Sink () () Int Int)
                Sink.collect
            )
            Stream.empty
        )
      r `shouldEqual` Tuple 0 []

    it "left-halted-early side reports its halt value, not its finish" do
      -- head halts after first element; last keeps consuming to the end
      r <- runRIO'
        (Sink.runSink (Sink.zipPar Sink.head Sink.last) source)
      r `shouldEqual` Tuple (Just 1) (Just 5)

  describe "zipParWith" do
    it "applies the combining function to the two results" do
      r <- runRIO'
        ( Sink.runSink
            ( Sink.zipParWith (\count total -> { count, total })
                Sink.count
                (Sink.foldL 0 (+))
            )
            source
        )
      r `shouldEqual` { count: 5, total: 15 }

  describe "Stream interop" do
    it "Sink.collect matches runCollect" do
      r1 <- runRIO' (Sink.runSink Sink.collect source)
      r2 <- runRIO' (Stream.runCollect source)
      r1 `shouldEqual` r2

    it "Sink.foldL matches runFold" do
      r1 <- runRIO' (Sink.runSink (Sink.foldL 100 (+)) source)
      r2 <- runRIO' (Stream.runFold 100 (+) source)
      r1 `shouldEqual` r2
