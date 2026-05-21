module Test.RIO.Aff.KeyedPoolSpec (spec) where

import Prelude

import Data.Array (sort)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (RIO, ask, provideAll, runRIO')
import RIO.Aff.KeyedPool as KP
import RIO.Aff.Resource (scoped)
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Aff.KeyedPool" do
  it "creates one pool per key on first use and reuses it after" do
    counts <- liftEffect (Ref.new (0 :: Int))
    let
      program
        :: RIO () ()
             { a1 :: Int, a2 :: Int, b1 :: Int, total :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        kp <- KP.make scope
          { acquire: \_ -> liftEffect (Ref.modify (_ + 1) counts)
          , release: \_ -> pure unit
          , capacity: \_ -> 2
          }
        a1 <- KP.withResource kp "a" pure
        a2 <- KP.withResource kp "a" pure
        b1 <- KP.withResource kp "b" pure
        total <- liftEffect (Ref.read counts)
        pure { a1, a2, b1, total }

    r <- runRIO' program
    r.a1 `shouldEqual` 1
    r.a2 `shouldEqual` 1
    r.b1 `shouldEqual` 2
    r.total `shouldEqual` 2

  it "caps concurrent borrowers per key independently" do
    active <- liftEffect (Ref.new (0 :: Int))
    peak <- liftEffect (Ref.new (0 :: Int))
    let
      borrow :: forall r. KP.KeyedPool String Unit -> String -> RIO r () Unit
      borrow kp k = KP.withResource kp k \_ -> do
        liftEffect do
          n <- Ref.modify (_ + 1) active
          p <- Ref.read peak
          when (n > p) (Ref.write n peak)
        liftAff (Aff.delay (Milliseconds 5.0))
        liftEffect (Ref.modify_ (_ - 1) active)

      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        kp <- KP.make scope
          { acquire: \_ -> pure unit
          , release: \_ -> pure unit
          , capacity: \_ -> 1
          }
        _ <- parTraverse (borrow kp) [ "a", "a", "a", "b", "b", "b" ]
        pure unit

    _ <- runRIO' program
    pk <- liftEffect (Ref.read peak)
    pk `shouldEqual` 2

  it "scope close releases every idle resource across keys" do
    destroyed <- liftEffect (Ref.new ([] :: Array String))
    let
      program :: RIO () () Unit
      program = do
        scoped do
          scope <- ask (Proxy :: Proxy "scope")
          kp <- KP.make scope
            { acquire: \k -> pure k
            , release: \r ->
                liftEffect (Ref.modify_ (\xs -> xs <> [ r ]) destroyed)
            , capacity: \_ -> 2
            }
          for_ [ "x", "y", "z" ] \k ->
            KP.withResource kp k \_ -> pure unit
        -- One tick for finalizers.
        liftAff (Aff.delay (Milliseconds 10.0))

    _ <- runRIO' program
    rs <- liftEffect (Ref.read destroyed)
    sort rs `shouldEqual` [ "x", "y", "z" ]

  it "keys reports every key that has been borrowed" do
    let
      program :: RIO () () (Array String)
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        kp <- KP.make scope
          { acquire: \k -> pure k
          , release: \_ -> pure unit
          , capacity: \_ -> 1
          }
        for_ [ "alpha", "beta", "gamma" ] \k ->
          KP.withResource kp k \_ -> pure unit
        ks <- KP.keys kp
        pure (Set.toUnfoldable ks)

    xs <- runRIO' program
    sort xs `shouldEqual` [ "alpha", "beta", "gamma" ]

  it "per-key capacity comes from the supplied function" do
    active <- liftEffect (Ref.new (0 :: Int))
    peak <- liftEffect (Ref.new (0 :: Int))
    let
      borrow :: forall r. KP.KeyedPool String Unit -> String -> RIO r () Unit
      borrow kp k = KP.withResource kp k \_ -> do
        liftEffect do
          n <- Ref.modify (_ + 1) active
          p <- Ref.read peak
          when (n > p) (Ref.write n peak)
        liftAff (Aff.delay (Milliseconds 5.0))
        liftEffect (Ref.modify_ (_ - 1) active)

      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        kp <- KP.make scope
          { acquire: \_ -> pure unit
          , release: \_ -> pure unit
          , capacity: \k -> if k == "hot" then 3 else 1
          }
        _ <- parTraverse (borrow kp)
          [ "hot", "hot", "hot", "hot", "hot" ]
        pure unit

    _ <- runRIO' program
    pk <- liftEffect (Ref.read peak)
    pk `shouldEqual` 3

  describe "withResource'" do
    it "invalidate triggers a remint on the next borrow for that key" do
      counter <- liftEffect (Ref.new (0 :: Int))
      let
        program :: RIO () () (Array Int)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          kp <- KP.make scope
            { acquire: \_ -> liftEffect (Ref.modify (_ + 1) counter)
            , release: \_ -> pure unit
            , capacity: \_ -> 1
            }
          first <- KP.withResource' kp "k" \r invalidate -> do
            invalidate
            pure r
          second <- KP.withResource' kp "k" \r _ -> pure r
          pure [ first, second ]
      xs <- runRIO' program
      xs `shouldEqual` [ 1, 2 ]

  describe "shutdown" do
    it "drains every per-key pool and lets fresh borrows rebuild lazily" do
      releaseLog <- liftEffect (Ref.new ([] :: Array String))
      counter <- liftEffect (Ref.new (0 :: Int))
      midLog <- liftEffect (Ref.new ([] :: Array String))
      let
        program :: RIO () () Int
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          kp <- KP.make scope
            { acquire: \k -> do
                _ <- liftEffect (Ref.modify (_ + 1) counter)
                pure k
            , release: \k ->
                liftEffect (Ref.modify_ (\xs -> xs <> [ k ]) releaseLog)
            , capacity: \_ -> 1
            }
          for_ [ "a", "b", "c" ] \k ->
            KP.withResource kp k \_ -> pure unit
          KP.shutdown kp
          -- Snapshot the release log right after shutdown but
          -- before the post-shutdown rebuild, so the assertion is
          -- pinned to the shutdown drain itself.
          snapshot <- liftEffect (Ref.read releaseLog)
          liftEffect (Ref.write snapshot midLog)
          -- After shutdown the key map is cleared; a new borrow
          -- rebuilds the pool for "a" lazily, minting a fresh
          -- resource.
          KP.withResource kp "a" \_ -> liftEffect (Ref.read counter)
      finalCount <- runRIO' program
      finalCount `shouldEqual` 4
      mid <- liftEffect (Ref.read midLog)
      sort mid `shouldEqual` [ "a", "b", "c" ]

  describe "makeWithTTL" do
    it "expires an idle resource per-key once its age exceeds the TTL" do
      counter <- liftEffect (Ref.new (0 :: Int))
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () { first :: Int, second :: Int }
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          kp <- KP.makeWithTTL scope
            { acquire: \_ -> liftEffect (Ref.modify (_ + 1) counter)
            , release: \_ -> pure unit
            , capacity: \_ -> 1
            , timeToLive: Milliseconds 100.0
            }
          first <- KP.withResource kp "k" pure
          liftAff (tc.advance (Milliseconds 200.0))
          second <- KP.withResource kp "k" pure
          pure { first, second }
      out <- runRIO' (provideAll { clock: tc.clock } program)
      out.first `shouldEqual` 1
      out.second `shouldEqual` 2

    it "reuses an idle resource that is still within the TTL" do
      counter <- liftEffect (Ref.new (0 :: Int))
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () { first :: Int, second :: Int }
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          kp <- KP.makeWithTTL scope
            { acquire: \_ -> liftEffect (Ref.modify (_ + 1) counter)
            , release: \_ -> pure unit
            , capacity: \_ -> 1
            , timeToLive: Milliseconds 1000.0
            }
          first <- KP.withResource kp "k" pure
          second <- KP.withResource kp "k" pure
          pure { first, second }
      out <- runRIO' (provideAll { clock: tc.clock } program)
      out.first `shouldEqual` 1
      out.second `shouldEqual` 1
