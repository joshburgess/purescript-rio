module Test.RIO.Aff.Stream.CombinatorsSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO)
import Data.Maybe (Maybe(..))

import RIO.Aff.Stream
  ( Stream
  , changes
  , chunk
  , distinct
  , distinctBy
  , dropWhile
  , flatten
  , fromArray
  , interleave
  , intersperse
  , paginate
  , paginateM
  , runCollect
  , scan
  , scanM
  , single
  , takeWhile
  , zip
  , zipWith
  , zipWithIndex
  )

spec :: Spec Unit
spec = describe "RIO.Aff.Stream combinators" do
  describe "zip / zipWith / zipWithIndex" do
    it "zips two equal-length streams elementwise" do
      let
        program :: RIO () () (Array (Tuple Int String))
        program = runCollect
          (zip (fromArray [ 1, 2, 3 ]) (fromArray [ "a", "b", "c" ]))
      result <- runRIO program
      result `shouldEqual`
        ( Right [ Tuple 1 "a", Tuple 2 "b", Tuple 3 "c" ]
            :: Either _ (Array (Tuple Int String))
        )

    it "zip ends when the left stream ends" do
      let
        program :: RIO () () (Array (Tuple Int String))
        program = runCollect
          (zip (fromArray [ 1, 2 ]) (fromArray [ "a", "b", "c", "d" ]))
      result <- runRIO program
      result `shouldEqual`
        ( Right [ Tuple 1 "a", Tuple 2 "b" ]
            :: Either _ (Array (Tuple Int String))
        )

    it "zip ends when the right stream ends" do
      let
        program :: RIO () () (Array (Tuple Int String))
        program = runCollect
          (zip (fromArray [ 1, 2, 3, 4 ]) (fromArray [ "a", "b" ]))
      result <- runRIO program
      result `shouldEqual`
        ( Right [ Tuple 1 "a", Tuple 2 "b" ]
            :: Either _ (Array (Tuple Int String))
        )

    it "zipWith applies the combine function" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          ( zipWith (+) (fromArray [ 1, 2, 3 ])
              (fromArray [ 10, 20, 30 ])
          )
      result <- runRIO program
      result `shouldEqual` (Right [ 11, 22, 33 ] :: Either _ (Array Int))

    it "zipWithIndex labels every element with its 0-based position" do
      let
        program :: RIO () () (Array (Tuple Int String))
        program = runCollect (zipWithIndex (fromArray [ "a", "b", "c" ]))
      result <- runRIO program
      result `shouldEqual`
        ( Right [ Tuple 0 "a", Tuple 1 "b", Tuple 2 "c" ]
            :: Either _ (Array (Tuple Int String))
        )

  describe "scan / scanM" do
    it "emits the seed first, then a running fold" do
      -- A pure prefix-sum: seed 0 over [1,2,3] yields [0,1,3,6].
      let
        program :: RIO () () (Array Int)
        program = runCollect (scan 0 (+) (fromArray [ 1, 2, 3 ]))
      result <- runRIO program
      result `shouldEqual` (Right [ 0, 1, 3, 6 ] :: Either _ (Array Int))

    it "scan on an empty stream yields just the seed" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (scan 42 (+) (fromArray [] :: Stream () () Int))
      result <- runRIO program
      result `shouldEqual` (Right [ 42 ] :: Either _ (Array Int))

    it "scanM threads an effectful accumulator" do
      -- The effectful step writes each acc to a ref so we can
      -- verify side-effect timing matches output emission.
      log <- liftEffect (Ref.new ([] :: Array Int))
      let
        program :: RIO () () (Array Int)
        program = runCollect
          ( scanM 0
              ( \acc x -> do
                  let acc' = acc + x
                  liftEffect (Ref.modify_ (\xs -> xs <> [ acc' ]) log)
                  pure acc'
              )
              (fromArray [ 1, 2, 3 ])
          )
      result <- runRIO program
      result `shouldEqual` (Right [ 0, 1, 3, 6 ] :: Either _ (Array Int))
      observed <- liftEffect (Ref.read log)
      observed `shouldEqual` [ 1, 3, 6 ]

  describe "chunk" do
    it "groups consecutive elements into arrays of size n" do
      let
        program :: RIO () () (Array (Array Int))
        program = runCollect (chunk 2 (fromArray [ 1, 2, 3, 4, 5 ]))
      result <- runRIO program
      -- Last chunk is shorter when input length is not a multiple
      -- of n.
      result `shouldEqual`
        ( Right [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]
            :: Either _ (Array (Array Int))
        )

    it "chunk on an empty stream yields no chunks" do
      let
        program :: RIO () () (Array (Array Int))
        program = runCollect (chunk 3 (fromArray [] :: Stream () () Int))
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array (Array Int)))

    it "chunk with n <= 0 yields an empty stream" do
      let
        program :: RIO () () (Array (Array Int))
        program = runCollect (chunk 0 (fromArray [ 1, 2, 3 ]))
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array (Array Int)))

  describe "takeWhile / dropWhile" do
    it "takeWhile stops at (and excludes) the first failing element" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (takeWhile (\x -> x < 3) (fromArray [ 1, 2, 3, 4, 1 ]))
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2 ] :: Either _ (Array Int))

    it "dropWhile drops the leading run, then includes the rest" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (dropWhile (\x -> x < 3) (fromArray [ 1, 2, 3, 4, 1 ]))
      result <- runRIO program
      result `shouldEqual` (Right [ 3, 4, 1 ] :: Either _ (Array Int))

    it "takeWhile on an empty stream is empty" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          ( takeWhile (\_ -> true) (fromArray [] :: Stream () () Int)
          )
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array Int))

  describe "intersperse" do
    it "inserts the separator between elements" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (intersperse 0 (fromArray [ 1, 2, 3 ]))
      result <- runRIO program
      result `shouldEqual`
        (Right [ 1, 0, 2, 0, 3 ] :: Either _ (Array Int))

    it "intersperse on a single-element stream emits that element only" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (intersperse 0 (single 42))
      result <- runRIO program
      result `shouldEqual` (Right [ 42 ] :: Either _ (Array Int))

    it "intersperse on an empty stream is empty" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (intersperse 0 (fromArray [] :: Stream () () Int))
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array Int))

  describe "flatten" do
    it "concatenates a stream of streams in order" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          ( flatten
              ( fromArray
                  [ fromArray [ 1, 2 ]
                  , fromArray [ 3 ]
                  , fromArray [ 4, 5 ]
                  ]
              )
          )
      result <- runRIO program
      result `shouldEqual`
        (Right [ 1, 2, 3, 4, 5 ] :: Either _ (Array Int))

  describe "distinct" do
    it "drops consecutive duplicates, keeps the first of each run" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (distinct (fromArray [ 1, 1, 2, 2, 2, 3, 1, 1 ]))
      result <- runRIO program
      -- The trailing `1` is kept because it follows a `3` - distinct
      -- only collapses consecutive duplicates.
      result `shouldEqual`
        (Right [ 1, 2, 3, 1 ] :: Either _ (Array Int))

    it "distinct on an empty stream is empty" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (distinct (fromArray [] :: Stream () () Int))
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array Int))

  describe "distinctBy" do
    it "collapses consecutive elements with equal keys, keeping the first" do
      let
        program :: RIO () () (Array (Tuple Int String))
        program = runCollect
          ( distinctBy (\(Tuple k _) -> k)
              ( fromArray
                  [ Tuple 1 "a", Tuple 1 "b", Tuple 2 "c", Tuple 2 "d"
                  , Tuple 3 "e", Tuple 1 "f"
                  ]
              )
          )
      result <- runRIO program
      result `shouldEqual`
        ( Right [ Tuple 1 "a", Tuple 2 "c", Tuple 3 "e", Tuple 1 "f" ]
            :: Either _ (Array (Tuple Int String))
        )

    it "distinctBy on an empty stream is empty" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (distinctBy identity (fromArray [] :: Stream () () Int))
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ (Array Int))

  describe "changes" do
    it "is an adjacency-only deduplicator, like distinct" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (changes (fromArray [ 1, 1, 2, 2, 1, 1 ]))
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 1 ] :: Either _ (Array Int))

  describe "paginate / paginateM" do
    it "paginate emits each page and stops when the next cursor is Nothing" do
      let
        -- Three pages, each carrying one value, then no more.
        step :: Int -> Tuple Int (Maybe Int)
        step n =
          if n >= 3 then Tuple n Nothing
          else Tuple n (Just (n + 1))

        program :: RIO () () (Array Int)
        program = runCollect (paginate 1 step)
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ (Array Int))

    it "paginateM threads RIO through every page request" do
      counter <- liftEffect (Ref.new 0)
      let
        step :: Int -> RIO () () (Tuple Int (Maybe Int))
        step n = do
          _ <- liftEffect (Ref.modify (_ + 1) counter)
          pure
            ( if n >= 3 then Tuple n Nothing
              else Tuple n (Just (n + 1))
            )

        program :: RIO () () (Array Int)
        program = runCollect (paginateM 1 step)
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ (Array Int))
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 3

  describe "interleave" do
    it "alternates left, right, left, right" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (interleave (fromArray [ 1, 2, 3 ]) (fromArray [ 10, 20, 30 ]))
      result <- runRIO program
      result `shouldEqual`
        (Right [ 1, 10, 2, 20, 3, 30 ] :: Either _ (Array Int))

    it "forwards the tail of the longer side once the shorter ends" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          (interleave (fromArray [ 1, 2 ]) (fromArray [ 10, 20, 30, 40 ]))
      result <- runRIO program
      -- After 1, 10, 2, 20 the left is exhausted; the rest of the
      -- right is then forwarded in order.
      result `shouldEqual`
        (Right [ 1, 10, 2, 20, 30, 40 ] :: Either _ (Array Int))

    it "interleave with an empty left forwards the right unchanged" do
      let
        program :: RIO () () (Array Int)
        program = runCollect
          ( interleave (fromArray [] :: Stream () () Int)
              (fromArray [ 1, 2, 3 ])
          )
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ (Array Int))
