-- | Per-operation-tag dispatch profiler.
-- |
-- | Runs each representative workload in isolation, brackets it with
-- | `resetOpCounts` / `dumpOpCounts`, and logs the tag counts
-- | alongside the iteration count so the mix per iteration is easy to
-- | read off. Enable the profiling interpreter build via
-- | `RIO_OP_PROFILE=1`; with that env var unset, all fields read
-- | zero.
module Benchmarks.Profile
  ( main
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Number.Format (toStringWith, fixed)
import Data.String (length) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import RIO.Aff.Core
  ( RIO
  , ask
  , catchAll
  , catchTag
  , fail
  , provideAll
  , runRIO
  , runRIO'
  )
import RIO.Aff.Internal (OpCounts, dumpOpCounts, resetOpCounts)
import Type.Proxy (Proxy(..))

type Service = { lookup :: Int -> Int }

stubService :: Service
stubService = { lookup: \n -> n + 1 }

bindChain :: forall r e. Int -> RIO r e Int
bindChain n = go 0 n
  where
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

serviceLoop :: forall r' e. Int -> RIO (svc :: Service | r') e Int
serviceLoop n = go 0 n
  where
  go acc 0 = pure acc
  go acc k = do
    svc <- ask (Proxy :: Proxy "svc")
    go (svc.lookup acc) (k - 1)

catchLoop :: forall r. Int -> RIO r () Int
catchLoop n = go 0 n
  where
  go acc 0 = pure acc
  go acc k =
    catchTag (Proxy :: Proxy "oops")
      (\(x :: Int) -> go (acc + x) (k - 1))
      (fail (Proxy :: Proxy "oops") (1 :: Int))

catchAllLoop :: forall r. Int -> RIO r () Int
catchAllLoop n = go 0 n
  where
  go acc 0 = pure acc
  go acc k =
    catchAll
      (\_ -> go (acc + 1) (k - 1))
      (fail (Proxy :: Proxy "oops") (1 :: Int))

refCounterLoop :: forall r e. Ref.Ref Int -> Int -> RIO r e Int
refCounterLoop ref n = go n
  where
  go 0 = liftEffect (Ref.read ref)
  go k = do
    _ <- liftEffect (Ref.modify (_ + 1) ref)
    go (k - 1)

main :: Effect Unit
main = launchAff_ profileAll

profileAll :: Aff Unit
profileAll = do
  liftEffect do
    log ""
    log "================================================================"
    log "  rio operation-tag dispatch profile"
    log "================================================================"
    log "  Set RIO_OP_PROFILE=1 to enable counting; otherwise all"
    log "  counts read zero."
    log ""

  let iters = 10000

  profile ("bind chain (" <> show iters <> " binds)") iters
    (void (runRIO' (bindChain iters)))

  profile ("service loop (" <> show iters <> " ask + lookup)") iters
    ( void
        ( runRIO
            (provideAll { svc: stubService } (serviceLoop iters))
        )
    )

  profile ("catch loop (" <> show iters <> " round-trips)") iters
    (void (runRIO' (catchLoop iters)))

  profile ("catchAll loop (" <> show iters <> " round-trips)") iters
    (void (runRIO' (catchAllLoop iters)))

  profile ("ref counter loop (" <> show iters <> " modifies)") iters
    ( void do
        ref <- liftEffect (Ref.new 0)
        runRIO' (refCounterLoop ref iters)
    )

profile :: String -> Int -> Aff Unit -> Aff Unit
profile label iters action = do
  liftEffect resetOpCounts
  action
  counts <- liftEffect dumpOpCounts
  liftEffect do
    log ("--- " <> label <> " ---")
    log ("  iterations: " <> show iters)
    logCounts counts iters

logCounts :: OpCounts -> Int -> Effect Unit
logCounts c iters = do
  let
    total =
      c."PURE" + c."SYNC" + c."BIND" + c."ASK" + c."FAIL"
        + c."CATCH"
        + c."LOCAL"
        + c."ASYNC"
        + c."LIFT"
        + c."SYNC_LIFT"
        + c."CATCH_ALL"
  log ("  total dispatches: " <> show total)
  for_
    [ Tuple "PURE" c."PURE"
    , Tuple "SYNC" c."SYNC"
    , Tuple "BIND" c."BIND"
    , Tuple "ASK" c."ASK"
    , Tuple "FAIL" c."FAIL"
    , Tuple "CATCH" c."CATCH"
    , Tuple "LOCAL" c."LOCAL"
    , Tuple "ASYNC" c."ASYNC"
    , Tuple "LIFT" c."LIFT"
    , Tuple "SYNC_LIFT" c."SYNC_LIFT"
    , Tuple "CATCH_ALL" c."CATCH_ALL"
    ]
    \(Tuple name count) -> do
      let perIter = toNumber count / toNumber iters
      log
        ( "  "
            <> padRight 12 name
            <> padLeft 10 (show count)
            <> "  ("
            <> toStringWith (fixed 2) perIter
            <> "/iter)"
        )

padRight :: Int -> String -> String
padRight width s = s <> spaces (width - String.length s)

padLeft :: Int -> String -> String
padLeft width s = spaces (width - String.length s) <> s

spaces :: Int -> String
spaces n = if n <= 0 then "" else " " <> spaces (n - 1)
