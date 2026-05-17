-- | Qualified-`ado` sugar for running independent `RIO` actions
-- | concurrently and combining their results.
-- |
-- | Each line of a `Par.ado` block runs as its own branch under
-- | `Control.Parallel`'s `ParAff`. The block returns once every
-- | branch completes:
-- |
-- | ```purescript
-- | import RIO.Concurrency.Par as Par
-- |
-- | fetchAll :: forall r e. UserId -> RIO r e Bundle
-- | fetchAll uid = Par.ado
-- |   user  <- fetchUser uid
-- |   prefs <- fetchPrefs uid
-- |   posts <- fetchPosts uid
-- |   in { user, prefs, posts }
-- | ```
-- |
-- | The three fetches run in parallel; wall-clock cost is
-- | roughly the slowest branch, not the sum.
-- |
-- | ## Semantics
-- |
-- | * **Applicative-only.** Use with `ado`, not `do`. Qualified
-- |   `do` would still sequence (`bind` for parallel composition
-- |   cannot exist without the second action waiting for the
-- |   first's value). Each `<-` in a `Par.ado` block must be
-- |   independent of the bindings above it.
-- |
-- | * **No short-circuit.** If one branch fails with a typed
-- |   error, the other branches still run to completion. The
-- |   final result is the leftmost typed failure. This makes
-- |   `Par.ado` safe for fan-out where each branch should always
-- |   be given a chance to run. For short-circuiting fan-out,
-- |   reach for `RIO.Concurrency.zipPar` (two branches) or
-- |   `RIO.Concurrency.parTraverse` (N branches), which cancel
-- |   the remaining branches the moment one branch fails.
-- |
-- | * **Defects.** A defect (`Aff` exception) in any branch
-- |   propagates; the other branches are interrupted by the
-- |   underlying `ParAff` runtime. Resources held via
-- |   `acquireRelease` or `Scope` are released as usual.
-- |
-- | * **Shared environment.** Each branch sees the same
-- |   environment record. There is no per-branch override.
module RIO.Concurrency.Par
  ( apply
  , map
  , pure
  ) where

import Prelude (bind, ($), (<$>), (<*>))
import Prelude (pure) as P

import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Functor (map) as F

import RIO.Internal (RIO(..), mkRIO, rioFail, unRIO)

-- | The qualified-`ado` desugaring target for the functorial
-- | step. Identical to the `Functor RIO` instance; mapping over
-- | a single result has no parallelism to expose.
map :: forall r e a b. (a -> b) -> RIO r e a -> RIO r e b
map = F.map

-- | The qualified-`ado` desugaring target for combining two
-- | independent branches. Both run concurrently via
-- | `Aff.parallel` / `Aff.sequential`.
-- |
-- | Failure bias: leftmost typed failure wins, but both branches
-- | are allowed to complete (no automatic interruption on first
-- | failure). See the module-level docs for why and what to
-- | reach for instead when you want short-circuiting.
apply
  :: forall r e a b
   . RIO r e (a -> b)
  -> RIO r e a
  -> RIO r e b
apply rf ra = mkRIO \r -> do
  result <- sequential
    ( combine
        <$> parallel (unRIO rf r)
        <*> parallel (unRIO ra r)
    )
  case result of
    Right b -> P.pure b
    Left v -> rioFail v
  where
  combine :: Either _ (a -> b) -> Either _ a -> Either _ b
  combine (Left v) _ = Left v
  combine _ (Left v) = Left v
  combine (Right f) (Right a) = Right $ f a

-- | Re-export of `Prelude.pure` so a `Par.ado` block whose
-- | `in` expression has no `<-` bindings still resolves through
-- | this module.
pure :: forall r e a. a -> RIO r e a
pure = P.pure
