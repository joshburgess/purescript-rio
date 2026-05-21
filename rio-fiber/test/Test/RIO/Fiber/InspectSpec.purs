module Test.RIO.Fiber.InspectSpec (spec) where

import Prelude hiding (join)

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO, fork, join, liftEffect)
import RIO.Fiber.Core as F
import RIO.Fiber.Inspect
  ( FiberStatus(..)
  , currentFiberId
  , currentLabel
  , dump
  , fiberLabel
  , fiberStatus
  , setFiberLabel
  , setLabel
  , unFiberId
  )

run :: forall a. RIO () () a -> Aff a
run = runAffThrow

spec :: Spec Unit
spec = describe "rio-fiber: Inspect" do
  it "assigns distinct ids to forked fibers" do
    { parent, child } <- run do
      parentId <- currentFiberId
      childFiber <- fork (currentFiberId :: RIO () () _)
      childId <- join childFiber
      pure { parent: parentId, child: childId }
    (parent == child) `shouldEqual` false

  it "the same fiber sees the same id on consecutive reads" do
    ids <- run do
      a <- currentFiberId
      _ <- join =<< fork (pure unit :: RIO () () Unit)
      b <- currentFiberId
      pure { a, b }
    ids.a `shouldEqual` ids.b

  it "currentLabel is Nothing until set" do
    label <- run currentLabel
    label `shouldEqual` Nothing

  it "setLabel + currentLabel roundtrip" do
    label <- run do
      setLabel "worker-1"
      currentLabel
    label `shouldEqual` Just "worker-1"

  it "setLabel inside a child does not affect the parent's label" do
    { parentLabel, childLabel } <- run do
      setLabel "parent"
      childFiber <- fork do
        setLabel "child"
        currentLabel
      cl <- join childFiber
      pl <- currentLabel
      pure { parentLabel: pl, childLabel: cl }
    parentLabel `shouldEqual` Just "parent"
    childLabel `shouldEqual` Just "child"

  it "fiberLabel and setFiberLabel work against a handle" do
    result <- run do
      f <- fork do
        F.sleep (Milliseconds 20.0)
        currentLabel
      liftEffect (setFiberLabel f "external")
      observed <- liftEffect (fiberLabel f)
      labelInside <- join f
      pure { observed, labelInside }
    result.observed `shouldEqual` Just "external"
    result.labelInside `shouldEqual` Just "external"

  it "fiberStatus reads Running before the fiber completes, Done after" do
    statuses <- run do
      ref <- liftEffect (Ref.new (Nothing :: Maybe FiberStatus))
      f <- fork do
        F.sleep (Milliseconds 20.0)
      pre <- liftEffect (fiberStatus f)
      liftEffect (Ref.write (Just pre) ref)
      _ <- join f
      post <- liftEffect (fiberStatus f)
      pure { pre, post }
    -- Pre-join the fiber should be either Running or Suspended (sleeping),
    -- both of which are non-Done. Post-join it must be Done.
    statuses.pre `shouldSatisfy` (_ /= Done)
    statuses.post `shouldEqual` Done

  it "dump captures id, label, and status" do
    out <- run do
      f <- fork do
        setLabel "worker"
        F.sleep (Milliseconds 20.0)
      F.sleep (Milliseconds 5.0)
      pre <- liftEffect (dump f)
      _ <- join f
      post <- liftEffect (dump f)
      pure { pre, post }
    out.pre.label `shouldEqual` Just "worker"
    out.post.status `shouldEqual` Done
    unFiberId out.pre.id `shouldEqual` unFiberId out.post.id
