-- | A `Cause` tree for richer error reporting.
-- |
-- | `RIO.Error` already distinguishes typed failures (`fail`,
-- | tracked in the row) from defects (`die`, surfaced through the
-- | underlying `Aff` exception channel). What it does not give you
-- | is a place to record "several things failed at the same time,
-- | here is the whole picture" - the standard `race` /
-- | `parTraverse` combinators expose only the first failure they
-- | see, which is good for short-circuit fan-out and not so good
-- | for after-the-fact debugging.
-- |
-- | A `Cause e` is a small algebra over those two atoms (`Fail`
-- | for a typed failure, `Die` for a defect) plus two structural
-- | combinators (`Parallel` for failures that occurred at the
-- | same time, `Sequential` for failures the program saw in
-- | order). `prettyCause` walks the tree and emits a multi-line
-- | summary suitable for printing to stderr or attaching to a
-- | bug report.
-- |
-- | The intent is the same as Effect-TS's `Cause` and ZIO's
-- | `Cause`: keep the typed-error story (`fail` payloads in the
-- | row) and the unstructured story (`Error` defects) cleanly
-- | separated, even when several of them happen at once.
-- |
-- | ```purescript
-- | -- collapse two parallel attempts into one report
-- | report <- bothPar fetchUser fetchPrefs
-- |   # map (case _ of
-- |       Right (Tuple u p) -> renderBundle u p
-- |       Left c -> prettyCause renderTypedFailure c)
-- | ```
module RIO.Cause
  ( Cause(..)
  , bothPar
  , prettyCause
  , fromOutcome
  , concatParallel
  , concatSequential
  ) where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Array (intercalate) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Aff (attempt)
import Effect.Exception (Error, message)
import Effect.Exception (error) as Exception

import RIO.Internal (RIO(..), unRIO)

-- | One node of the failure tree.
-- |
-- |   * `Fail v` carries a typed failure (`Variant e`).
-- |   * `Die err` carries a defect (an `Aff` exception, or
-- |     anything raised through `RIO.Error.die`).
-- |   * `Parallel a b` records failures that happened at the same
-- |     time (under `race`, `parTraverse`, or `bothPar`).
-- |   * `Sequential a b` records failures the program observed in
-- |     order (under `do`-style sequencing, when a later step
-- |     fails in the cleanup path of an earlier one).
data Cause e
  = Fail (Variant e)
  | Die Error
  | Parallel (Cause e) (Cause e)
  | Sequential (Cause e) (Cause e)

-- | Concatenate two causes as parallel.
concatParallel :: forall e. Cause e -> Cause e -> Cause e
concatParallel = Parallel

-- | Concatenate two causes as sequential.
concatSequential :: forall e. Cause e -> Cause e -> Cause e
concatSequential = Sequential

-- | Convert the standard `Either Error (Either (Variant e) a)`
-- | outcome shape (a defect on the outer `Left`, a typed failure
-- | on the inner `Left`) into either a `Cause` (if anything went
-- | wrong) or a value (if it succeeded). This is the shape
-- | `attempt (unRIO m r)` produces inside the library.
fromOutcome
  :: forall e a
   . Either Error (Either (Variant e) a)
  -> Either (Cause e) a
fromOutcome = case _ of
  Right (Right a) -> Right a
  Right (Left v) -> Left (Fail v)
  Left err -> Left (Die err)

-- | Run two effects in parallel. If both succeed, returns their
-- | tuple. If exactly one fails, returns the failing cause. If
-- | both fail, returns the parallel combination of both causes.
-- |
-- | Unlike `RIO.Concurrency.race` (which short-circuits on the
-- | first completion) and `Concurrency.Par.ado` (which only
-- | surfaces the leftmost typed failure), `bothPar` runs both
-- | sides to completion and reports every failure it sees.
bothPar
  :: forall r e a b
   . RIO r e a
  -> RIO r e b
  -> RIO r e (Either (Cause e) (Tuple a b))
bothPar ra rb = RIO \r -> do
  -- Run both sides under `attempt` so defects don't tear the
  -- whole pair down before we have both outcomes.
  Tuple oa ob <- sequential
    ( Tuple <$> parallel (attempt (unRIO ra r))
        <*> parallel (attempt (unRIO rb r))
    )
  let
    causeA = case oa of
      Right (Right _) -> Nothing
      Right (Left v) -> Just (Fail v)
      Left err -> Just (Die err)
    causeB = case ob of
      Right (Right _) -> Nothing
      Right (Left v) -> Just (Fail v)
      Left err -> Just (Die err)
  pure $ Right $ case causeA, causeB, oa, ob of
    Nothing, Nothing, Right (Right a), Right (Right b) ->
      Right (Tuple a b)
    Just c, Nothing, _, _ -> Left c
    Nothing, Just c, _, _ -> Left c
    Just c1, Just c2, _, _ -> Left (Parallel c1 c2)
    -- Unreachable: causeX = Nothing iff the corresponding outcome
    -- is Right (Right _). The compiler can't see that through the
    -- two separate `case` scrutinees, hence the explicit fallback.
    _, _, _, _ -> Left
      (Die (Exception.error "RIO.Cause.bothPar: impossible"))

-- | Render a `Cause` as a multi-line, human-readable tree.
-- |
-- | The caller supplies a renderer for typed failures (`Variant e
-- | -> String`) because `Variant` does not have a generic `Show`.
-- | Defects are rendered via `Effect.Exception.message`.
-- |
-- | The output format:
-- |
-- |   * Atomic causes render on a single line.
-- |   * `Parallel` introduces a header and indents both branches.
-- |   * `Sequential` does the same, with a different header so
-- |     readers can tell the two apart.
prettyCause :: forall e. (Variant e -> String) -> Cause e -> String
prettyCause renderFail = go 0
  where
  pad :: Int -> String
  pad n = repeatStr n "  "

  go :: Int -> Cause e -> String
  go depth = case _ of
    Fail v -> pad depth <> "fail: " <> renderFail v
    Die err -> pad depth <> "defect: " <> message err
    Parallel a b ->
      pad depth <> "parallel failures:\n"
        <> Array.intercalate "\n"
          [ go (depth + 1) a, go (depth + 1) b ]
    Sequential a b ->
      pad depth <> "sequenced failures:\n"
        <> Array.intercalate "\n"
          [ go (depth + 1) a, go (depth + 1) b ]

-- A tiny utility kept private so the module stays self-contained.
repeatStr :: Int -> String -> String
repeatStr n s
  | n <= 0 = ""
  | otherwise = s <> repeatStr (n - 1) s
