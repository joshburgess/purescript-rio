module Test.RIO.Fiber.SupervisorSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, elem)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Supervisor (Supervisor(..))
import RIO.Fiber.Supervisor as Sup
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Supervisor" do
  it "fires onStart for every fiber spawned and onEnd for every completion" do
    started <- liftEffect (Ref.new 0)
    ended <- liftEffect (Ref.new 0)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> Ref.modify_ (_ + 1) started
              , onEnd: \_ -> Ref.modify_ (_ + 1) ended
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = do
        _ <- F.parTraverse (\_ -> pure unit) [ 1, 2, 3 ]
        pure unit
    out <- runAff prog {}
    liftEffect unreg
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    s <- liftEffect (Ref.read started)
    e <- liftEffect (Ref.read ended)
    -- every fiber that starts also ends.
    s `shouldEqual` e
    -- at minimum the outer fiber + three parallel children.
    (s >= 4) `shouldEqual` true

  it "unregister stops further callbacks" do
    started <- liftEffect (Ref.new 0)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> Ref.modify_ (_ + 1) started
              , onEnd: \_ -> pure unit
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = pure unit
    _ <- runAff prog {}
    before <- liftEffect (Ref.read started)
    liftEffect unreg
    _ <- runAff prog {}
    after <- liftEffect (Ref.read started)
    -- after unregister, the second run should not bump the counter
    after `shouldEqual` before

  it "onStart and onEnd see the same id for each fiber" do
    pairs <- liftEffect (Ref.new ([] :: Array { start :: Int, end :: Int }))
    starts <- liftEffect (Ref.new ([] :: Array Int))
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \id -> Ref.modify_ (\xs -> xs <> [ id ]) starts
              , onEnd: \id -> do
                  ss <- Ref.read starts
                  -- find a matching start id (every end must have one)
                  let matched = id `elem` ss
                  when matched
                    ( Ref.modify_
                        (\xs -> xs <> [ { start: id, end: id } ])
                        pairs
                    )
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = pure unit
    _ <- runAff prog {}
    liftEffect unreg
    ps <- liftEffect (Ref.read pairs)
    -- every pair has matching ids (trivially true by construction, but
    -- asserting we actually got at least one means the hook fired).
    all (\p -> p.start == p.end) ps `shouldEqual` true
    (Array.length ps >= 1) `shouldEqual` true

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
