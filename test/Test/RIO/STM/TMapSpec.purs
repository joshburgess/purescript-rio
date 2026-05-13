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
