module Test.RIO.Stream.ConcurrentSpec (spec) where

import Prelude hiding (join)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Concurrency (fork, join, zipPar)
import RIO.Core (RIO, fail, runRIO, scoped)
import RIO.Resource (Scope)
import RIO.Stream (Stream, fromArray, mapM, runCollect)
import RIO.Stream.Concurrent (broadcastDynamic)

spec :: Spec Unit
spec = describe "RIO.Stream.Concurrent" do

  describe "broadcastDynamic" do

    it "two subscribers each see every element from a slow source" do
      -- Make the source slow enough that both subscribers attach
      -- before any element has been published. With an instant
      -- source (`fromArray` with no delay), the first subscriber
      -- might race the publisher and miss the leading elements;
      -- the slow source pins the behaviour to the "both attached
      -- before publication" case the docstring describes.
      let
        program :: RIO () () (Tuple (Array Int) (Array Int))
        program = scoped do
          let
            source :: Stream (scope :: Scope | ()) () Int
            source = mapM
              ( \n -> do
                  liftAff (delay (Milliseconds 5.0))
                  pure n
              )
              (fromArray [ 1, 2, 3 ])
          subscribe <- broadcastDynamic source
          s1 <- subscribe
          s2 <- subscribe
          zipPar (runCollect s1) (runCollect s2)
      r <- runRIO program
      case r of
        Right (Tuple xs ys) -> do
          xs `shouldEqual` [ 1, 2, 3 ]
          ys `shouldEqual` [ 1, 2, 3 ]
        Left _ ->
          Spec.fail "expected dynamic broadcast to succeed"

    it "a late subscriber after source completion sees an empty stream" do
      let
        program :: RIO () () (Array Int)
        program = scoped do
          subscribe <- broadcastDynamic (fromArray [ 1, 2, 3 ])
          -- Give the publisher time to drain and shut the hub.
          liftAff (delay (Milliseconds 20.0))
          s <- subscribe
          runCollect s
      r <- runRIO program
      r `shouldEqual` Right []

    it "subscriber sees the typed failure raised by the source" do
      let
        program :: RIO () (boom :: String) (Array Int)
        program = scoped do
          let
            source :: Stream (scope :: Scope | ()) (boom :: String) Int
            source = mapM
              ( \_ -> do
                  liftAff (delay (Milliseconds 5.0))
                  fail (Proxy :: Proxy "boom") "kaboom"
              )
              (fromArray [ 1 ])
          subscribe <- broadcastDynamic source
          s <- subscribe
          runCollect s
      r <- runRIO program
      case r of
        Left _ -> pure unit
        Right _ ->
          Spec.fail "expected subscriber to surface the typed failure"

    it "two subscribers attached at different times see disjoint suffixes" do
      -- Each value published before a subscriber attaches is not
      -- replayed for that subscriber: docstring promise that
      -- "earlier values are not replayed". Pin it by interleaving
      -- a subscribe with a publication: the second subscriber
      -- should miss the leading elements.
      let
        program :: RIO () () (Tuple (Array Int) (Array Int))
        program = scoped do
          let
            -- 4 items, 5ms apart, so the second subscribe at
            -- ~10ms misses elements 1 and 2 but sees 3 and 4.
            source :: Stream (scope :: Scope | ()) () Int
            source = mapM
              ( \n -> do
                  liftAff (delay (Milliseconds 5.0))
                  pure n
              )
              (fromArray [ 1, 2, 3, 4 ])
          subscribe <- broadcastDynamic source
          s1 <- subscribe
          f1 <- fork (runCollect s1)
          liftAff (delay (Milliseconds 10.0))
          s2 <- subscribe
          f2 <- fork (runCollect s2)
          xs <- join f1
          ys <- join f2
          pure (Tuple xs ys)
      r <- runRIO program
      case r of
        Right (Tuple xs ys) -> do
          xs `shouldEqual` [ 1, 2, 3, 4 ]
          -- The late subscriber must see a non-empty suffix that
          -- is in-order and contained in the source. Exact split
          -- depends on scheduling, but `4` is reliable.
          (Array.length ys >= 1) `shouldEqual` true
          (Array.last ys) `shouldEqual` Just 4
        Left _ ->
          Spec.fail "expected dynamic broadcast to succeed"
