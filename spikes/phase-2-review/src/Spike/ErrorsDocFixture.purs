-- | Inference fixture for `docs/03-errors.md`.
-- |
-- | The doc walks a program with three possible errors down to zero,
-- | citing the compiler's inferred type at each step. This module is
-- | the source of truth for those inferred types: every step is
-- | unannotated and the compiler reports them via
-- | `MissingTypeDeclaration` warnings on a fresh build.
-- |
-- | If a change to the error primitives shifts these inferred types,
-- | the warnings change here and the doc goes out of sync.
-- |
-- | Reproducing the warnings:
-- |
-- | ```
-- | rm -rf output
-- | npx spago build -p spike-phase-2-review 2>&1 | grep -A 18 'inferred type of step'
-- | ```
module Spike.ErrorsDocFixture where

import Prelude

import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, catchTag, fail, runRIO')

-- The three failure tags the doc starts from.
type ThreeErrors =
  ( notFound :: { id :: Int }
  , parse :: String
  , unauthorized :: Unit
  )

-- The starting program. We pin its row explicitly so the walkthrough
-- has a fixed point of reference. From here down, every binding is
-- unannotated and the compiler reports its inferred type.
step0 :: forall r. RIO r ThreeErrors Int
step0 = do
  _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
  _ <- fail (Proxy :: Proxy "parse") "bad json"
  fail (Proxy :: Proxy "unauthorized") unit

-- step1: handle `notFound`. The expected inferred row drops
-- `notFound` and keeps `parse` and `unauthorized`.
step1 = catchTag (Proxy :: Proxy "notFound") (\_ -> pure 0) step0

-- step2: handle `parse`. Only `unauthorized` remains.
step2 = catchTag (Proxy :: Proxy "parse") (\_ -> pure (-1)) step1

-- step3: handle `unauthorized`. The error row should now be exactly
-- `()`, so the program is runnable with `runRIO'`.
step3 = catchTag (Proxy :: Proxy "unauthorized") (\_ -> pure (-2)) step2

-- Demonstration that step3's error row really is `()`: it type-checks
-- under `runRIO'`, whose signature requires `RIO () () a`.
runStep3 = runRIO' step3
