module Test.RIO.Aff.Stream.SlidingGroupConsSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Aff.Stream (cons / append / sliding / groupBy)" do

  describe "cons" do
    it "yields the prepended element first, then the original stream" do
      let
        s :: Stream () () Int
        s = Stream.cons 0 (Stream.fromArray [ 1, 2, 3 ])

        program :: RIO () () (Array Int)
        program = Stream.runCollect s
      result <- runRIO' program
      result `shouldEqual` [ 0, 1, 2, 3 ]

    it "yields a singleton when prepended onto an empty stream" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.cons 7 (Stream.empty :: Stream () () Int))
      result <- runRIO' program
      result `shouldEqual` [ 7 ]

  describe "append" do
    it "yields the original stream first, then the appended element" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.append (Stream.fromArray [ 1, 2, 3 ]) 99)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3, 99 ]

    it "yields a singleton when appending onto an empty stream" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.append (Stream.empty :: Stream () () Int) 42)
      result <- runRIO' program
      result `shouldEqual` [ 42 ]

  describe "sliding" do
    it "yields every length-n window in order" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.sliding 3 (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
      result <- runRIO' program
      result `shouldEqual`
        [ [ 1, 2, 3 ]
        , [ 2, 3, 4 ]
        , [ 3, 4, 5 ]
        ]

    it "yields a single window when the stream length equals n" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.sliding 3 (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual` [ [ 1, 2, 3 ] ]

    it "yields nothing when the stream is shorter than n" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.sliding 5 (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual` []

    it "yields nothing when n <= 0" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.sliding 0 (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual` []

    it "treats n = 1 as a per-element window" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.sliding 1 (Stream.fromArray [ 9, 8, 7 ]))
      result <- runRIO' program
      result `shouldEqual` [ [ 9 ], [ 8 ], [ 7 ] ]

  describe "groupBy" do
    it "groups runs of equal elements" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.groupBy (==) (Stream.fromArray [ 1, 1, 2, 2, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual`
        [ [ 1, 1 ]
        , [ 2, 2, 2 ]
        , [ 3 ]
        ]

    it "emits one chunk when every element relates to its neighbour" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.groupBy (\_ _ -> true) (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual` [ [ 1, 2, 3 ] ]

    it "emits singletons when the relation never holds" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.groupBy (\_ _ -> false) (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO' program
      result `shouldEqual` [ [ 1 ], [ 2 ], [ 3 ] ]

    it "compares against the immediately preceding element" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          ( Stream.groupBy (\a b -> b == a + 1)
              (Stream.fromArray [ 1, 2, 3, 10, 11, 20 ])
          )
      result <- runRIO' program
      result `shouldEqual`
        [ [ 1, 2, 3 ]
        , [ 10, 11 ]
        , [ 20 ]
        ]

    it "yields an empty stream on empty input" do
      let
        program :: RIO () () (Array (Array Int))
        program = Stream.runCollect
          (Stream.groupBy (==) (Stream.empty :: Stream () () Int))
      result <- runRIO' program
      result `shouldEqual` []
