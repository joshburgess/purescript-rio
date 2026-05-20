module Test.RIO.Fiber.SupervisorSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, elem)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Supervisor (Supervisor(..))
import RIO.Fiber.Supervisor as Sup
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

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

  it "fires onEnd when a fiber fails with a typed error" do
    ended <- liftEffect (Ref.new 0)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> pure unit
              , onEnd: \_ -> Ref.modify_ (_ + 1) ended
              }
          )
      )
    let
      prog :: F.RIO () (boom :: String) Unit
      prog = F.fail (Variant.inj (Proxy :: _ "boom") "x")
    out <- runAff prog {}
    liftEffect unreg
    case out of
      Fail _ -> pure unit
      other -> fail ("expected Fail, got " <> describeOutcome other)
    n <- liftEffect (Ref.read ended)
    -- at least the outer fiber that ran prog should have ended.
    (n >= 1) `shouldEqual` true

  it "fires onEnd when a fiber dies with a defect" do
    ended <- liftEffect (Ref.new 0)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> pure unit
              , onEnd: \_ -> Ref.modify_ (_ + 1) ended
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = F.die (error "fatal")
    out <- runAff prog {}
    liftEffect unreg
    case out of
      Die _ -> pure unit
      other -> fail ("expected Die, got " <> describeOutcome other)
    n <- liftEffect (Ref.read ended)
    (n >= 1) `shouldEqual` true

  it "fires onEnd when a fiber is interrupted" do
    ended <- liftEffect (Ref.new 0)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> pure unit
              , onEnd: \_ -> Ref.modify_ (_ + 1) ended
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = do
        f <- F.fork (F.sleep (Milliseconds 1000.0))
        F.sleep (Milliseconds 5.0)
        F.interrupt f
        -- `join` of an interrupted child propagates `Interrupted`
        -- to the parent; `causeOf` absorbs the cause so the parent
        -- still finishes successfully.
        _ <- F.causeOf (F.join f)
        pure unit
    out <- runAff prog {}
    liftEffect unreg
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    n <- liftEffect (Ref.read ended)
    -- at minimum the interrupted child should have ended; the
    -- parent fiber is still running when we read this counter so
    -- it may or may not be included.
    (n >= 1) `shouldEqual` true

  it "multiple supervisors all see every event" do
    countA <- liftEffect (Ref.new 0)
    countB <- liftEffect (Ref.new 0)
    unregA <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> Ref.modify_ (_ + 1) countA
              , onEnd: \_ -> pure unit
              }
          )
      )
    unregB <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> Ref.modify_ (_ + 1) countB
              , onEnd: \_ -> pure unit
              }
          )
      )
    let
      prog :: F.RIO () () Unit
      prog = do
        _ <- F.parTraverse (\_ -> pure unit) [ 1, 2 ]
        pure unit
    _ <- runAff prog {}
    liftEffect unregA
    liftEffect unregB
    a <- liftEffect (Ref.read countA)
    b <- liftEffect (Ref.read countB)
    a `shouldEqual` b
    (a >= 3) `shouldEqual` true

  it "throwing in onStart does not crash the running fiber" do
    -- A misbehaving supervisor: onStart throws. The runtime is
    -- documented to swallow exceptions from supervisor hooks.
    sawEnd <- liftEffect (Ref.new false)
    unreg <- liftEffect
      ( Sup.register
          ( Supervisor
              { onStart: \_ -> do
                  -- Intentional throw via Effect's `throw`.
                  _ <- pure unit
                  throwIt
              , onEnd: \_ -> Ref.write true sawEnd
              }
          )
      )
    let
      prog :: F.RIO () () Int
      prog = pure 42
    out <- runAff prog {}
    liftEffect unreg
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)
    e <- liftEffect (Ref.read sawEnd)
    e `shouldEqual` true

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

throwIt :: Effect Unit
throwIt = throwException (error "supervisor hook throw")

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
