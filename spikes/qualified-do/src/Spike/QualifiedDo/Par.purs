-- | Qualified-do candidate: `Par.ado` runs each `<-` line
-- | concurrently and collects the results.
-- |
-- | ```purescript
-- | import Spike.QualifiedDo.Par as Par
-- |
-- | example = Par.ado
-- |   user  <- fetchUser uid
-- |   prefs <- fetchPrefs uid
-- |   posts <- fetchPosts uid
-- |   in { user, prefs, posts }
-- | ```
-- |
-- | The three fetches run in parallel under `Control.Parallel`'s
-- | `ParAff`. The block returns once all three complete.
-- |
-- | Semantics (intentionally simple; tradeoffs documented in the
-- | spike's FINDINGS):
-- |
-- |   * No short-circuit: if one branch fails with a typed error,
-- |     the other branches still run to completion. The final
-- |     result is the leftmost typed failure. (Compare
-- |     `RIO.Aff.Concurrency.zipPar`, which short-circuits via Aff
-- |     interruption.)
-- |
-- |   * Defects (`Aff` exceptions) propagate from whichever branch
-- |     raised first; the others are interrupted by `ParAff`.
-- |
-- |   * Each branch sees the same environment record.
-- |
-- | Use only with `ado`, not `do`: `Par.do` would still sequence,
-- | because qualified-do's `<-` desugars to `Par.bind` and bind
-- | for an applicative computation cannot be implemented without
-- | losing the parallelism (the second action would have to wait
-- | for the first to deliver its value).
module Spike.QualifiedDo.Par
  ( apply
  , map
  , pure
  ) where

import Prelude (($), (<$>), (<*>), (>>=))
import Prelude (pure) as P

import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Functor (map) as F

import RIO.Aff.Internal (RIO, mkRIO, rioFail, unRIO)

-- | `Par.map`: the qualified-`ado` desugaring target for the
-- | functorial step. Identical to the `Functor RIO` instance;
-- | mapping over a single result has no parallelism to expose.
map :: forall r e a b. (a -> b) -> RIO r e a -> RIO r e b
map = F.map

-- | `Par.apply`: the qualified-`ado` desugaring target for
-- | combining two independent branches. Both run concurrently
-- | via `Aff.parallel` / `Aff.sequential`.
-- |
-- | Failure bias: leftmost typed failure wins, but both branches
-- | are allowed to run to completion. There is no automatic
-- | cancellation; see the module docs for why.
apply
  :: forall r e a b
   . RIO r e (a -> b)
  -> RIO r e a
  -> RIO r e b
apply rf ra = mkRIO \r ->
  sequential
    ( combine
        <$> parallel (unRIO rf r)
        <*> parallel (unRIO ra r)
    )
    >>= case _ of
      Left v -> rioFail v
      Right b -> P.pure b
  where
  combine :: Either _ (a -> b) -> Either _ a -> Either _ b
  combine (Left v) _ = Left v
  combine _ (Left v) = Left v
  combine (Right f) (Right a) = Right $ f a

-- | Re-export of `Prelude.pure` so `Par.ado` blocks that build a
-- | value with `in expr` (no `<-` lines) resolve through this
-- | module.
pure :: forall r e a. a -> RIO r e a
pure = P.pure
