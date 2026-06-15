module Test.RIO.Aff.ReloadableSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import RIO.Aff.Clock (Clock, liveClock)
import RIO.Aff.Core (RIO, ask, fail, provideAll, runRIO)
import RIO.Aff.Error (either)
import RIO.Aff.Reloadable as Reloadable
import RIO.Aff.Resource (scoped)
import RIO.Aff.Schedule (recurs, spaced)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "RIO.Aff.Reloadable" do
  it "seeds the initial value at make time" do
    acquires <- liftEffect (ERef.new 0)
    let
      acquire :: forall r. RIO r () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      program :: RIO (clock :: Clock) () { acquires :: Int, value :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        slot <- Reloadable.make scope (recurs 0) acquire
        v <- Reloadable.get slot
        a <- liftEffect (ERef.read acquires)
        pure { acquires: a, value: v }
    result <- runRIO (provideAll { clock: liveClock } program)
    result `shouldEqual`
      (Right { acquires: 1, value: 1 } :: Either _ _)

  it "manual reload re-runs acquire immediately" do
    acquires <- liftEffect (ERef.new 0)
    let
      acquire :: forall r. RIO r () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      program :: RIO (clock :: Clock) () { before :: Int, after :: Int, calls :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        -- recurs 0 returns Done immediately (zero Continue steps).
        -- The forked loop completes immediately; manual reload drives
        -- the only observable re-acquire here.
        slot <- Reloadable.make scope (recurs 0) acquire
        before <- Reloadable.get slot
        Reloadable.reload slot
        after <- Reloadable.get slot
        calls <- liftEffect (ERef.read acquires)
        pure { before, after, calls }
    result <- runRIO (provideAll { clock: liveClock } program)
    case result of
      Right rec -> do
        rec.before `shouldEqual` 1
        rec.after `shouldEqual` 2
        -- Seed + manual reload account for 2 calls; the recurs-0
        -- forked loop may opportunistically fire one extra acquire
        -- before the scope is torn down, so allow >= 2.
        if rec.calls >= 2 then pure unit
        else 0 `shouldEqual` 1
      Left _ -> 1 `shouldEqual` 0

  it "manual reload re-raises acquire failures" do
    callsRef <- liftEffect (ERef.new 0)
    let
      acquire :: forall r. RIO r Boom Int
      acquire = do
        n <- liftEffect
          (ERef.modify' (\k -> { state: k + 1, value: k + 1 }) callsRef)
        if n == 1 then pure 7
        else fail (Proxy :: Proxy "boom") "later"

      program :: RIO (clock :: Clock) Boom { initial :: Int, reloadFailed :: Boolean, after :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        slot <- Reloadable.make scope (recurs 0) acquire
        initial <- Reloadable.get slot
        outcome <- either (Reloadable.reload slot)
        let reloadFailed = case outcome of
              Left _ -> true
              Right _ -> false
        after <- Reloadable.get slot
        pure { initial, reloadFailed, after }
    result <- runRIO (provideAll { clock: liveClock } program)
    case result of
      Right rec -> do
        rec.initial `shouldEqual` 7
        rec.reloadFailed `shouldEqual` true
        rec.after `shouldEqual` 7
      Left _ -> 1 `shouldEqual` 0

  it "scheduled re-acquires update the slot" do
    acquires <- liftEffect (ERef.new 0)
    let
      acquire :: forall r. RIO r () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      program :: RIO (clock :: Clock) () { initial :: Int, later :: Int, calls :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        slot <- Reloadable.make scope (spaced (Milliseconds 5.0)) acquire
        initial <- Reloadable.get slot
        liftAff (delay (Milliseconds 40.0))
        later <- Reloadable.get slot
        calls <- liftEffect (ERef.read acquires)
        pure { initial, later, calls }
    result <- runRIO (provideAll { clock: liveClock } program)
    case result of
      Right rec -> do
        rec.initial `shouldEqual` 1
        if rec.later > rec.initial then pure unit
        else 0 `shouldEqual` 1
        if rec.calls >= 2 then pure unit
        else 0 `shouldEqual` 1
      Left _ -> 1 `shouldEqual` 0

  it "scheduled-loop failures are swallowed and the slot keeps its prior value" do
    acquires <- liftEffect (ERef.new 0)
    let
      acquire :: forall r. RIO r Boom Int
      acquire = do
        n <- liftEffect
          (ERef.modify' (\k -> { state: k + 1, value: k + 1 }) acquires)
        if n == 1 then pure 100
        else fail (Proxy :: Proxy "boom") ("tick " <> show n)

      program :: RIO (clock :: Clock) Boom { value :: Int, calls :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        slot <- Reloadable.make scope (spaced (Milliseconds 5.0)) acquire
        liftAff (delay (Milliseconds 40.0))
        v <- Reloadable.get slot
        calls <- liftEffect (ERef.read acquires)
        pure { value: v, calls }
    result <- runRIO (provideAll { clock: liveClock } program)
    case result of
      Right rec -> do
        rec.value `shouldEqual` 100
        if rec.calls >= 2 then pure unit
        else 0 `shouldEqual` 1
      Left _ -> 1 `shouldEqual` 0
