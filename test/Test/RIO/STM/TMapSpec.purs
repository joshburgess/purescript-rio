module Test.RIO.STM.TMapSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, fork, join, runRIO')
import RIO.STM (atomically)
import RIO.STM.TMap
  ( awaitKey
  , deleteTMap
  , insertTMap
  , lookupTMap
  , memberTMap
  , newTMap
  , sizeTMap
  )

spec :: Spec Unit
spec = describe "RIO.STM.TMap" do
  it "insert + lookup round-trips" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (insertTMap 1 100 m)
        atomically (lookupTMap 1 m)
    result <- runRIO' program
    result `shouldEqual` Just 100

  it "deleteTMap removes a key" do
    let
      program :: RIO () () { before :: Boolean, after :: Boolean }
      program = do
        m <- atomically (newTMap :: _ (_ Int String))
        atomically (insertTMap 7 "seven" m)
        before <- atomically (memberTMap 7 m)
        atomically (deleteTMap 7 m)
        after <- atomically (memberTMap 7 m)
        pure { before, after }
    result <- runRIO' program
    result `shouldEqual` { before: true, after: false }

  it "sizeTMap reports the count" do
    let
      program :: RIO () () Int
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (insertTMap 1 1 m)
        atomically (insertTMap 2 2 m)
        atomically (insertTMap 3 3 m)
        atomically (sizeTMap m)
    result <- runRIO' program
    result `shouldEqual` 3

  it "awaitKey blocks until the key is inserted" do
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () String
      program = do
        m <- atomically (newTMap :: _ (_ Int String))
        push "before-fork"
        waiter <- fork do
          v <- atomically (awaitKey 42 m)
          push "after-await"
          pure v
        liftAff (delay (Milliseconds 20.0))
        push "before-insert"
        atomically (insertTMap 42 "found-it" m)
        join waiter
    result <- runRIO' program
    result `shouldEqual` "found-it"
    order <- liftEffect (Ref.read events)
    order `shouldEqual` [ "before-fork", "before-insert", "after-await" ]

  it "lookupTMap on a missing key returns Nothing" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (insertTMap 1 10 m)
        atomically (lookupTMap 2 m)
    result <- runRIO' program
    result `shouldEqual` Nothing

  it "insertTMap of an existing key overwrites" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (insertTMap 1 10 m)
        atomically (insertTMap 1 20 m)
        atomically (lookupTMap 1 m)
    result <- runRIO' program
    result `shouldEqual` Just 20

  it "deleteTMap of an absent key is a no-op" do
    let
      program :: RIO () () { sizeBefore :: Int, sizeAfter :: Int }
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (insertTMap 1 10 m)
        sizeBefore <- atomically (sizeTMap m)
        atomically (deleteTMap 99 m)
        sizeAfter <- atomically (sizeTMap m)
        pure { sizeBefore, sizeAfter }
    result <- runRIO' program
    result `shouldEqual` { sizeBefore: 1, sizeAfter: 1 }

  it "sizeTMap on an empty map is 0" do
    let
      program :: RIO () () Int
      program = do
        m <- atomically (newTMap :: _ (_ Int Int))
        atomically (sizeTMap m)
    result <- runRIO' program
    result `shouldEqual` 0

  it "awaitKey returns immediately when the key is already present" do
    let
      program :: RIO () () String
      program = do
        m <- atomically (newTMap :: _ (_ Int String))
        atomically (insertTMap 1 "ready" m)
        atomically (awaitKey 1 m)
    result <- runRIO' program
    result `shouldEqual` "ready"

  it "awaitKey re-checks (and stays blocked) on writes to unrelated keys" do
    -- The docstring promises that `awaitKey` wakes up "when any
    -- write to the underlying TRef fires (so an insert of a
    -- different key will re-check; this is the standard STM
    -- wakeup model, not an indexed one)". Pin the observable
    -- consequence: unrelated inserts must not cause the awaiter
    -- to resume early or return a wrong value, and a later
    -- insert of the awaited key must still resolve it correctly.
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () String
      program = do
        m <- atomically (newTMap :: _ (_ Int String))
        push "before-fork"
        waiter <- fork do
          v <- atomically (awaitKey 42 m)
          push "after-await"
          pure v
        liftAff (delay (Milliseconds 10.0))
        push "insert-1"
        atomically (insertTMap 1 "one" m)
        liftAff (delay (Milliseconds 10.0))
        push "insert-2"
        atomically (insertTMap 2 "two" m)
        liftAff (delay (Milliseconds 10.0))
        push "insert-42"
        atomically (insertTMap 42 "found-it" m)
        join waiter
    result <- runRIO' program
    result `shouldEqual` "found-it"
    order <- liftEffect (Ref.read events)
    order `shouldEqual`
      [ "before-fork"
      , "insert-1"
      , "insert-2"
      , "insert-42"
      , "after-await"
      ]
