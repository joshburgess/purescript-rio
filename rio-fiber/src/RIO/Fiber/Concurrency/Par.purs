-- | Qualified-`ado` sugar for running independent `RIO` actions
-- | concurrently and combining their results.
-- |
-- | Each line of a `Par.ado` block runs as its own branch on a
-- | freshly forked fiber. The block returns once every branch has
-- | settled:
-- |
-- | ```purescript
-- | import RIO.Fiber.Concurrency.Par as Par
-- |
-- | fetchAll :: forall r e. UserId -> RIO r e Bundle
-- | fetchAll uid = Par.ado
-- |   user  <- fetchUser uid
-- |   prefs <- fetchPrefs uid
-- |   posts <- fetchPosts uid
-- |   in { user, prefs, posts }
-- | ```
-- |
-- | The three fetches run in parallel; wall-clock cost is roughly the
-- | slowest branch, not the sum.
-- |
-- | ## Semantics
-- |
-- |   * **Applicative-only.** Use with `ado`, not `do`. Qualified
-- |     `do` would still sequence (`bind` for parallel composition
-- |     cannot exist without the second action waiting for the
-- |     first's value). Each `<-` in a `Par.ado` block must be
-- |     independent of the bindings above it.
-- |
-- |   * **No short-circuit.** If one branch fails (typed error,
-- |     defect, or interrupt), the other branches still run to
-- |     completion. The final outcome is the leftmost failure if
-- |     either branch failed; otherwise the combined value. This
-- |     makes `Par.ado` safe for fan-out where each branch should
-- |     always be given a chance to run. For short-circuiting
-- |     fan-out, reach for `RIO.Fiber.Core.zipPar` (two branches)
-- |     or `RIO.Fiber.Core.parTraverse` (N branches), which
-- |     interrupt the remaining branches the moment one fails.
-- |
-- |   * **Defects and interrupts.** Both surface through the
-- |     leftmost-failure rule: if `rf` died or was interrupted,
-- |     that cause is re-raised even if `ra` succeeded. Resources
-- |     held by either branch via `acquireRelease` / `Scope` are
-- |     released as usual.
-- |
-- |   * **Shared environment.** Each branch sees the same
-- |     environment record. There is no per-branch override.
module RIO.Fiber.Concurrency.Par
  ( apply
  , map
  , pure
  ) where

import Prelude (bind, identity, (<$>))
import Prelude (pure) as P

import Data.Either (Either(..))
import Data.Functor (map) as F
import Effect.Exception (error)

import RIO.Fiber.Core (RIO, causeOf, die, failCause, parTraverse)

-- | The qualified-`ado` desugaring target for the functorial step.
-- | Identical to the `Functor RIO` instance; mapping over a single
-- | result has no parallelism to expose.
map :: forall r e a b. (a -> b) -> RIO r e a -> RIO r e b
map = F.map

-- | The qualified-`ado` desugaring target for combining two
-- | independent branches. Both run concurrently on freshly forked
-- | fibers.
-- |
-- | Failure bias: leftmost failure wins, but both branches are
-- | allowed to complete (no automatic interruption on first
-- | failure). Each branch is wrapped in `causeOf`, which converts
-- | its own failure (typed, defect, or interrupt) into a value, so
-- | the underlying `parTraverse` never sees a failure to short-
-- | circuit on. The collected pair is then folded back to either a
-- | combined success or the leftmost cause via `failCause`.
apply
  :: forall r e a b
   . RIO r e (a -> b)
  -> RIO r e a
  -> RIO r e b
apply rf ra = do
  results <- parTraverse identity
    [ Left <$> causeOf rf
    , Right <$> causeOf ra
    ]
  case results of
    [ Left rfRes, Right raRes ] -> case rfRes of
      Left c -> failCause c
      Right f -> case raRes of
        Left c -> failCause c
        Right a -> P.pure (f a)
    _ -> die (error "RIO.Fiber.Concurrency.Par.apply: parTraverse invariant violated")

-- | Re-export of `Prelude.pure` so a `Par.ado` block whose
-- | `in` expression has no `<-` bindings still resolves through
-- | this module.
pure :: forall r e a. a -> RIO r e a
pure = P.pure
