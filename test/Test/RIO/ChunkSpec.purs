module Test.RIO.ChunkSpec (spec) where

import Prelude hiding (map, append)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Chunk
  ( Chunk
  , append
  , empty
  , foldl
  , foldr
  , fromArray
  , head
  , index
  , isEmpty
  , length
  , map
  , materialize
  , prepend
  , singleton
  , tail
  , toArray
  )

spec :: Spec Unit
spec = describe "RIO.Chunk" do
  describe "constructors and toArray" do
    it "empty has length 0 and roundtrips to []" do
      length (empty :: Chunk Int) `shouldEqual` 0
      toArray (empty :: Chunk Int) `shouldEqual` []
      isEmpty (empty :: Chunk Int) `shouldEqual` true

    it "singleton wraps one value" do
      length (singleton 7) `shouldEqual` 1
      toArray (singleton 7) `shouldEqual` [ 7 ]
      isEmpty (singleton 7) `shouldEqual` false

    it "fromArray wraps an array losslessly" do
      let c = fromArray [ 1, 2, 3, 4 ]
      length c `shouldEqual` 4
      toArray c `shouldEqual` [ 1, 2, 3, 4 ]

    it "fromArray [] is structurally empty" do
      isEmpty (fromArray ([] :: Array Int)) `shouldEqual` true

  describe "Semigroup / Monoid" do
    it "<> concatenates" do
      toArray (fromArray [ 1, 2 ] <> fromArray [ 3, 4 ])
        `shouldEqual` [ 1, 2, 3, 4 ]

    it "left identity: mempty <> c == c" do
      let c = fromArray [ 1, 2 ]
      toArray (mempty <> c) `shouldEqual` toArray c

    it "right identity: c <> mempty == c" do
      let c = fromArray [ 1, 2 ]
      toArray (c <> mempty) `shouldEqual` toArray c

    it "associative: (a <> b) <> c == a <> (b <> c)" do
      let
        a = fromArray [ 1 ]
        b = fromArray [ 2, 3 ]
        c = fromArray [ 4, 5, 6 ]
      toArray ((a <> b) <> c) `shouldEqual` toArray (a <> (b <> c))

    it "concatenation caches length without re-walking" do
      let
        big = fromArray (Array.replicate 1000 0)
        joined = big <> big <> big <> big
      length joined `shouldEqual` 4000

  describe "prepend / append" do
    it "prepend adds a value at the front" do
      toArray (prepend 0 (fromArray [ 1, 2, 3 ]))
        `shouldEqual` [ 0, 1, 2, 3 ]

    it "append adds a value at the back" do
      toArray (append (fromArray [ 1, 2, 3 ]) 4)
        `shouldEqual` [ 1, 2, 3, 4 ]

  describe "index / head / tail" do
    it "index returns Just at in-range positions" do
      let c = fromArray [ 10, 20, 30 ]
      index 0 c `shouldEqual` Just 10
      index 1 c `shouldEqual` Just 20
      index 2 c `shouldEqual` Just 30

    it "index returns Nothing out of range" do
      let c = fromArray [ 10, 20, 30 ]
      index (-1) c `shouldEqual` Nothing
      index 3 c `shouldEqual` Nothing

    it "index works across Concat nodes" do
      let
        c = fromArray [ 1, 2 ]
          <> fromArray [ 3, 4 ]
          <> fromArray [ 5, 6 ]
      index 0 c `shouldEqual` Just 1
      index 3 c `shouldEqual` Just 4
      index 5 c `shouldEqual` Just 6
      index 6 c `shouldEqual` Nothing

    it "head returns the first value or Nothing" do
      head (fromArray [ 9, 8, 7 ]) `shouldEqual` Just 9
      head (empty :: Chunk Int) `shouldEqual` Nothing

    it "tail drops the first value" do
      toArray (tail (fromArray [ 1, 2, 3 ])) `shouldEqual` [ 2, 3 ]
      toArray (tail (singleton 1)) `shouldEqual` ([] :: Array Int)
      toArray (tail (empty :: Chunk Int)) `shouldEqual` ([] :: Array Int)

  describe "map" do
    it "maps element-wise" do
      toArray (map (_ * 2) (fromArray [ 1, 2, 3 ]))
        `shouldEqual` [ 2, 4, 6 ]

    it "maps across Concat nodes" do
      let c = fromArray [ 1 ] <> fromArray [ 2, 3 ]
      toArray (map (_ + 10) c) `shouldEqual` [ 11, 12, 13 ]

    it "identity law: map id == id" do
      let c = fromArray [ 1, 2, 3 ] <> fromArray [ 4, 5 ]
      toArray (map identity c) `shouldEqual` toArray c

  describe "folds" do
    it "foldl sums the elements" do
      foldl (+) 0 (fromArray [ 1, 2, 3, 4 ]) `shouldEqual` 10

    it "foldl is left-associative" do
      foldl (\acc x -> acc <> show x) "" (fromArray [ 1, 2, 3 ])
        `shouldEqual` "123"

    it "foldr builds an array right-to-left" do
      foldr (\x acc -> [ x ] <> acc) [] (fromArray [ 1, 2, 3 ])
        `shouldEqual` [ 1, 2, 3 ]

    it "folds traverse Concat nodes in order" do
      let c = fromArray [ 1, 2 ] <> fromArray [ 3 ] <> fromArray [ 4, 5 ]
      foldl (\acc x -> acc <> [ x ]) [] c
        `shouldEqual` [ 1, 2, 3, 4, 5 ]

  describe "materialize" do
    it "preserves contents" do
      let c = fromArray [ 1 ] <> fromArray [ 2, 3 ] <> fromArray [ 4 ]
      toArray (materialize c) `shouldEqual` toArray c
