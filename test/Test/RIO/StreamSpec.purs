module Test.RIO.StreamSpec (spec) where

import Prelude

import Data.Array (range) as Array
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
import RIO.Stream
  ( Stream
  , concat
  , drop
  , empty
  , filter
  , flatMap
  , fromArray
  , map
  , mapM
  , repeatM
  , runCollect
  , runDrain
  , runFold
  , runFoldM
  , single
  , take
  , unfoldM
  )

spec :: Spec Unit
spec = do
  describe "RIO.Stream" do

    describe "construction" do
      it "fromArray yields every element in input order" do
        r <- runRIO' (runCollect (fromArray [ 1, 2, 3 ]))
        r `shouldEqual` [ 1, 2, 3 ]

      it "empty yields nothing" do
        r <- runRIO' (runCollect (empty :: Stream () () Int))
        r `shouldEqual` []

      it "single yields exactly one element" do
        r <- runRIO' (runCollect (single 42))
        r `shouldEqual` [ 42 ]

      it "unfoldM stops on Nothing" do
        let
          s = unfoldM 0 \n ->
            if n >= 3 then pure Nothing
            else pure (Just (Tuple n (n + 1)))
        r <- runRIO' (runCollect s)
        r `shouldEqual` [ 0, 1, 2 ]

      it "repeatM produces an unbounded source bounded by take" do
        counter <- liftEffect (Ref.new 0)
        let
          tick :: RIO () () Int
          tick = liftEffect (Ref.modify (_ + 1) counter)
        r <- runRIO' (runCollect (take 4 (repeatM tick)))
        r `shouldEqual` [ 1, 2, 3, 4 ]
        n <- liftEffect (Ref.read counter)
        n `shouldEqual` 4

    describe "transforms" do
      it "map applies a pure function" do
        r <- runRIO' (runCollect (map (_ * 2) (fromArray [ 1, 2, 3 ])))
        r `shouldEqual` [ 2, 4, 6 ]

      it "filter drops elements that fail the predicate" do
        r <- runRIO'
          ( runCollect
              ( filter (\n -> n `mod` 2 == 0)
                  (fromArray [ 1, 2, 3, 4, 5 ])
              )
          )
        r `shouldEqual` [ 2, 4 ]

      it "mapM threads effects in order" do
        log <- liftEffect (Ref.new ([] :: Array Int))
        let
          program :: RIO () () (Array Int)
          program = runCollect
            ( mapM
                ( \n -> do
                    liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) log)
                    pure (n + 100)
                )
                (fromArray [ 1, 2, 3 ])
            )
        r <- runRIO' program
        seen <- liftEffect (Ref.read log)
        r `shouldEqual` [ 101, 102, 103 ]
        seen `shouldEqual` [ 1, 2, 3 ]

    describe "slicing" do
      it "take stops after n elements" do
        r <- runRIO'
          (runCollect (take 3 (fromArray (Array.range 1 10))))
        r `shouldEqual` [ 1, 2, 3 ]

      it "take 0 yields an empty stream without pulling from the source" do
        -- `take`'s guard `n <= 0 = empty` short-circuits the
        -- source before any pull occurs; every existing `take`
        -- test passes a positive `n`, so a regression that
        -- weakened the guard to `n < 0 = empty` (or removed it
        -- entirely) would still pass every other test by
        -- pulling exactly one step before returning `Done` for
        -- `n = 0`. Pin the zero-pull invariant with a counting
        -- source: if the guard regressed, the counter would
        -- show one effect.
        counter <- liftEffect (Ref.new 0)
        let
          tick :: RIO () () Int
          tick = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO () () (Array Int)
          program = runCollect (take 0 (repeatM tick))
        r <- runRIO' program
        r `shouldEqual` ([] :: Array Int)
        pulls <- liftEffect (Ref.read counter)
        pulls `shouldEqual` 0

      it "drop discards the first n" do
        r <- runRIO'
          (runCollect (drop 3 (fromArray (Array.range 1 6))))
        r `shouldEqual` [ 4, 5, 6 ]

    describe "composition" do
      it "concat drains the first then the second" do
        r <- runRIO'
          ( runCollect
              ( concat (fromArray [ 1, 2 ])
                  (fromArray [ 3, 4 ])
              )
          )
        r `shouldEqual` [ 1, 2, 3, 4 ]

      it "flatMap replaces and concatenates" do
        let
          program :: RIO () () (Array Int)
          program = runCollect
            ( flatMap (fromArray [ 1, 2, 3 ])
                ( \n ->
                    fromArray [ n, n * 10 ]
                )
            )
        r <- runRIO' program
        r `shouldEqual` [ 1, 10, 2, 20, 3, 30 ]

    describe "runners" do
      it "runDrain visits each element" do
        log <- liftEffect (Ref.new ([] :: Array Int))
        let
          s = mapM
            (\n -> liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) log))
            (fromArray [ 5, 6, 7 ])
        runRIO' (runDrain s)
        seen <- liftEffect (Ref.read log)
        seen `shouldEqual` [ 5, 6, 7 ]

      it "runFold accumulates with a pure function" do
        r <- runRIO'
          (runFold 0 (+) (fromArray [ 1, 2, 3, 4 ]))
        r `shouldEqual` 10

      it "runFoldM accumulates with an effectful function" do
        r <- runRIO'
          ( runFoldM 0
              (\acc n -> pure (acc + n))
              (fromArray [ 1, 2, 3, 4 ])
          )
        r `shouldEqual` 10

    describe "typed-failure propagation" do
      -- The pull-based pipeline propagates a typed failure raised
      -- inside any effectful step (mapM, runFoldM, ...) on the
      -- parent's row, and short-circuits the remaining stream so
      -- later elements are never visited.
      it "mapM failure surfaces and halts the pipeline" do
        visited <- liftEffect (Ref.new ([] :: Array Int))
        let
          program :: RIO () (boom :: Int) (Array Int)
          program = runCollect
            ( mapM
                ( \n -> do
                    liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) visited)
                    if n == 2 then fail (Proxy :: Proxy "boom") n
                    else pure (n * 10)
                )
                (fromArray [ 1, 2, 3, 4 ])
            )
        result <- runRIO program
        seen <- liftEffect (Ref.read visited)
        case result of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected mapM failure to surface"
        seen `shouldEqual` [ 1, 2 ]

      it "runFoldM failure surfaces and halts the fold" do
        visited <- liftEffect (Ref.new ([] :: Array Int))
        let
          program :: RIO () (boom :: Int) Int
          program = runFoldM 0
            ( \acc n -> do
                liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) visited)
                if n == 3 then fail (Proxy :: Proxy "boom") n
                else pure (acc + n)
            )
            (fromArray [ 1, 2, 3, 4 ])
        result <- runRIO program
        seen <- liftEffect (Ref.read visited)
        case result of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected runFoldM failure to surface"
        seen `shouldEqual` [ 1, 2, 3 ]
