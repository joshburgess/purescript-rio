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
  , acquireReleaseCause
  , attemptCause
  , bothPar
  , catchAllCause
  , catchSomeCause
  , containsFailure
  , defects
  , failCause
  , failures
  , find
  , foldCause
  , foldCauseRIO
  , hasDefect
  , hasFailure
  , isFailure
  , linearize
  , prettyCause
  , prettyCauseWithStack
  , fromOutcome
  , concatParallel
  , concatSequential
  , parTraverseCause
  , parSequenceCause
  , raceCause
  , tapErrorCause
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Parallel (parallel, sequential)
import Control.Parallel (parTraverse) as Parallel
import Data.Array (foldl, intercalate, mapMaybe, uncons) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (attempt, bracket)
import Effect.Class (liftEffect)
import Data.String (joinWith, split) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect.Exception (Error, message, stack)
import Effect.Exception (error) as Exception
import Effect.Ref as Ref

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

-- | Run an `RIO` action and reify its outcome as `Either (Cause e) a`.
-- |
-- | This is the foundational primitive for cause-aware error
-- | handling: it converts the standard `attempt`-style outcome into
-- | a `Cause` leaf without changing the underlying semantics. A
-- | typed failure becomes `Fail v`, a defect becomes `Die err`.
-- |
-- | The caller's error row is left polymorphic because the result
-- | never carries a typed failure on the outer channel.
-- |
-- | ```purescript
-- | outcome <- attemptCause (fetchUser uid)
-- | case outcome of
-- |   Right user -> useUser user
-- |   Left cause -> logCause cause
-- | ```
attemptCause
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (Either (Cause e) a)
attemptCause action = RIO \r -> do
  outcome <- attempt (unRIO action r)
  pure (Right (fromOutcome outcome))

-- | The cause-aware analogue of `RIO.Error.foldRIO`: branch on the
-- | full `Cause` (every typed failure *and* every defect, including
-- | the structural `Parallel` / `Sequential` shapes) on the failure
-- | side, or on the success value on the other side.
-- |
-- | Unlike `foldRIO`, which only sees `Variant e` (typed failures)
-- | and lets defects propagate, `foldCauseRIO` reifies the underlying
-- | `Aff` exception channel into the `Die` leaf so handlers can react
-- | to defects without going through `sandbox` first. The output's
-- | error row `e'` is whatever the handler arms produce.
-- |
-- | ```purescript
-- | -- log every failure (typed or defect) with the same renderer
-- | foldCauseRIO
-- |   (\cause -> logInfo (prettyCause renderFail cause) *> pure fallback)
-- |   pure
-- |   runQuery
-- | ```
foldCauseRIO
  :: forall r e e' a b
   . (Cause e -> RIO r e' b)
  -> (a -> RIO r e' b)
  -> RIO r e a
  -> RIO r e' b
foldCauseRIO onCause onOk inner = RIO \r -> do
  outcome <- attempt (unRIO inner r)
  case fromOutcome outcome of
    Right a -> unRIO (onOk a) r
    Left cause -> unRIO (onCause cause) r

-- | Catch every failure (typed or defect) with a single handler and
-- | replace the error row. The cause-aware sibling of
-- | `RIO.Error.catchAll`: where `catchAll` only sees typed failures
-- | (`Variant e`) and lets defects propagate, `catchAllCause` sees
-- | the full `Cause` and can recover from defects too.
-- |
-- | Mirrors ZIO `ZIO.catchAllCause` / Effect-TS `Effect.catchAllCause`.
-- | Useful when "convert any kind of failure to a default" is what the
-- | call site wants and the distinction between typed failure and
-- | defect doesn't matter for the recovery strategy.
-- |
-- | ```purescript
-- | -- collapse every cause into a fallback value
-- | safeProgram :: RIO r () Int
-- | safeProgram = catchAllCause (\_ -> pure 0) program
-- | ```
catchAllCause
  :: forall r e e' a
   . (Cause e -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
catchAllCause handler inner = foldCauseRIO handler pure inner

-- | The cause-aware sibling of `RIO.Error.catchSome`: examine the
-- | full `Cause` and decide per failure whether to handle it
-- | (`Just (handler …)` recovers) or let it propagate unchanged
-- | (`Nothing` re-raises).
-- |
-- | Unlike `catchAllCause`, the error row is preserved: handled
-- | failures are discharged but unhandled ones flow through `e`.
-- | Unhandled composites are re-raised by picking their leftmost
-- | leaf (matching `failCause`'s policy), so the structural shape
-- | survives only through `catchAllCause`.
-- |
-- | Mirrors ZIO `ZIO.catchSomeCause`. Use this when "log unknown
-- | defects but pass them through" or "recover from a specific tag
-- | but keep the rest" is the recovery story.
catchSomeCause
  :: forall r e a
   . (Cause e -> Maybe (RIO r e a))
  -> RIO r e a
  -> RIO r e a
catchSomeCause classify inner = RIO \r -> do
  outcome <- attempt (unRIO inner r)
  case fromOutcome outcome of
    Right a -> pure (Right a)
    Left cause -> case classify cause of
      Just handler -> unRIO handler r
      Nothing -> unRIO (failCause cause) r

-- | Raise a pre-built `Cause` as a failure.
-- |
-- | For atomic causes the behaviour is exact: `Fail v` becomes a
-- | typed failure on the row, `Die err` becomes a defect on the
-- | underlying `Aff` channel. For composite causes (`Parallel`,
-- | `Sequential`) the surface error model is lossy: only one leaf
-- | can be raised at a time, so the leftmost leaf is selected. The
-- | structural information is preserved if the caller round-trips
-- | through `attemptCause` / `catchAllCause`, but it is *not*
-- | preserved through `runRIO` / `runRIO'`.
-- |
-- | Mirrors ZIO `ZIO.failCause`. The typical use is "re-raise a
-- | cause that was just observed" inside a `foldCauseRIO` /
-- | `catchAllCause` handler.
failCause :: forall r e a. Cause e -> RIO r e a
failCause cause = case leftmostLeaf cause of
  Fail v -> RIO \_ -> pure (Left v)
  Die err -> RIO \_ -> throwError err
  -- Unreachable: leftmostLeaf always returns Fail or Die.
  _ -> RIO \_ ->
    throwError (Exception.error "RIO.Cause.failCause: impossible")

-- | The leftmost atomic leaf of a cause tree. Used by `failCause` to
-- | pick a single leaf to raise when the cause is composite.
leftmostLeaf :: forall e. Cause e -> Cause e
leftmostLeaf = case _ of
  Fail v -> Fail v
  Die err -> Die err
  Parallel a _ -> leftmostLeaf a
  Sequential a _ -> leftmostLeaf a

-- | Run a side-effecting handler on the full `Cause` and re-raise the
-- | original failure unchanged. The cause-aware sibling of
-- | `RIO.Error.tapError`: where `tapError` only fires on typed
-- | failures, `tapErrorCause` fires on every failure (typed or defect).
-- |
-- | Mirrors ZIO `ZIO.tapErrorCause`. If the handler itself fails,
-- | *that* failure replaces the original (same policy as `tapError`
-- | and `tapBoth`). Successes pass through without invoking the
-- | handler.
-- |
-- | ```purescript
-- | runJob
-- |   # tapErrorCause (\c -> logError (prettyCause renderFail c))
-- | ```
tapErrorCause
  :: forall r e a
   . (Cause e -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapErrorCause f inner = RIO \r -> do
  outcome <- attempt (unRIO inner r)
  case fromOutcome outcome of
    Right a -> pure (Right a)
    Left cause -> do
      logged <- unRIO (f cause) r
      case logged of
        Left newFail -> pure (Left newFail)
        Right _ -> case cause of
          Fail v -> pure (Left v)
          Die err -> throwError err
          _ -> case leftmostLeaf cause of
            Fail v -> pure (Left v)
            Die err -> throwError err
            -- Unreachable: leftmostLeaf returns Fail or Die.
            _ -> throwError
              (Exception.error "RIO.Cause.tapErrorCause: impossible")

-- | Like `RIO.Concurrency.parTraverse`, but every branch runs to
-- | completion under `attempt` and every failure is collected into a
-- | left-leaning `Parallel` cause tree. No branch is short-circuited
-- | when a sibling fails.
-- |
-- | If every branch succeeds, returns `Right` of the result array
-- | in the original input order. If any branch fails (typed or
-- | defect), returns `Left` of a `Cause` that captures *every*
-- | failure observed, not just the first.
-- |
-- | This is the combinator the `Cause` renderer was written for:
-- | use it when "tell me everything that broke" matters more than
-- | "tell me as fast as possible". For first-failure-wins fan-out,
-- | reach for `RIO.Concurrency.parTraverse` instead.
-- |
-- | ```purescript
-- | outcome <- parTraverseCause validate inputs
-- | case outcome of
-- |   Right validated -> useAll validated
-- |   Left cause -> Console.log (prettyCause showFailure cause)
-- | ```
parTraverseCause
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Either (Cause e) (Array b))
parTraverseCause f as = RIO \r -> do
  outcomes <- Parallel.parTraverse
    (\a -> attempt (unRIO (f a) r))
    as
  let
    classified = map fromOutcome outcomes
    failed = Array.mapMaybe
      ( case _ of
          Left c -> Just c
          Right _ -> Nothing
      )
      classified
    successes = Array.mapMaybe
      ( case _ of
          Right b -> Just b
          Left _ -> Nothing
      )
      classified
  pure $ Right $ case combineParallel failed of
    Nothing -> Right successes
    Just cause -> Left cause

-- | The identity case of `parTraverseCause`: run an array of actions
-- | concurrently, collect every failure into a `Parallel` cause, and
-- | return all successes only if every branch succeeded.
parSequenceCause
  :: forall r e e' a
   . Array (RIO r e a)
  -> RIO r e' (Either (Cause e) (Array a))
parSequenceCause = parTraverseCause identity

-- | Race two actions: the first one to *succeed* wins. If both fail,
-- | the combined `Parallel` cause is returned. Unlike
-- | `RIO.Concurrency.race` (which surfaces the first completion,
-- | success or failure), `raceCause` waits for at least one success
-- | before giving up on the other side, so a fast failure does not
-- | beat a slow success.
-- |
-- | Both branches run under `attempt`; defects on either side are
-- | captured as `Die` rather than propagated as `Aff` exceptions.
-- |
-- | ```purescript
-- | -- prefer the primary cache, but fall back to the backup; only
-- | -- fail if *both* caches fail
-- | result <- raceCause (fromPrimary key) (fromBackup key)
-- | ```
raceCause
  :: forall r e e' a
   . RIO r e a
  -> RIO r e a
  -> RIO r e' (Either (Cause e) a)
raceCause ra rb = RIO \r -> do
  let
    runSide side = do
      outcome <- attempt (unRIO side r)
      case fromOutcome outcome of
        Right a -> pure (Right a)
        Left c -> pure (Left c)
  Tuple oa ob <- sequential
    ( Tuple <$> parallel (runSide ra)
        <*> parallel (runSide rb)
    )
  pure $ Right $ case oa, ob of
    Right a, _ -> Right a
    _, Right b -> Right b
    Left cA, Left cB -> Left (Parallel cA cB)

-- | Cause-aware bracket. Like `RIO.Resource.acquireRelease`, but if
-- | the body and the release *both* fail, the result is a
-- | `Sequential` cause that pairs the body's failure with the
-- | finalizer's, rather than silently dropping the finalizer
-- | exception while propagating the body's.
-- |
-- | The release retains the same row `()` as the regular
-- | `acquireRelease`: typed errors have nowhere to surface, but
-- | defects (whether raised by `die` or by the underlying `Aff`)
-- | are captured as `Die` leaves and combined into the cause tree.
-- |
-- | Acquire failures short-circuit before any release runs, just
-- | like the existing primitive: a failure during acquire becomes
-- | a single `Fail` / `Die` cause and the use / release phases are
-- | skipped entirely.
-- |
-- | The release is run through `Aff.bracket`'s uninterruptible
-- | release phase, so a kill landing during the body is queued
-- | until the release completes.
-- |
-- | ```purescript
-- | -- ensure the handle is closed even if the writer fails, and
-- | -- record both failures if the close itself blows up
-- | outcome <- acquireReleaseCause
-- |   (openHandle path)
-- |   (\h -> closeHandle h)
-- |   (\h -> writeBatch h batch)
-- | ```
acquireReleaseCause
  :: forall r e e' a b
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> (a -> RIO r e b)
  -> RIO r e' (Either (Cause e) b)
acquireReleaseCause acquire release use = RIO \r -> do
  acqOutcome <- attempt (unRIO acquire r)
  case acqOutcome of
    Left err -> pure (Right (Left (Die err)))
    Right (Left v) -> pure (Right (Left (Fail v)))
    Right (Right a) -> do
      releaseRef <- liftEffect
        (Ref.new (Right unit :: Either Error Unit))
      useOutcome <- attempt
        ( bracket
            (pure unit)
            ( \_ -> do
                ro <- attempt (unRIO (release a) r)
                case ro of
                  Right (Right _) -> pure unit
                  Right (Left v) -> Variant.case_ v
                  Left err ->
                    liftEffect (Ref.write (Left err) releaseRef)
            )
            (\_ -> unRIO (use a) r)
        )
      releaseResult <- liftEffect (Ref.read releaseRef)
      let
        useCause = case useOutcome of
          Right (Right _) -> Nothing
          Right (Left v) -> Just (Fail v)
          Left err -> Just (Die err)
        releaseCause = case releaseResult of
          Right _ -> Nothing
          Left err -> Just (Die err)
      pure $ Right $ case useOutcome, useCause, releaseCause of
        Right (Right b), _, Nothing -> Right b
        Right (Right _), _, Just rc -> Left rc
        _, Just uc, Nothing -> Left uc
        _, Just uc, Just rc -> Left (Sequential uc rc)
        _, _, _ -> Left
          ( Die
              ( Exception.error
                  "RIO.Cause.acquireReleaseCause: impossible"
              )
          )

-- | Fold an array of causes into a single `Parallel` tree. Returns
-- | `Nothing` if the array is empty, the cause itself for a
-- | singleton, and a left-leaning `Parallel` chain otherwise.
combineParallel :: forall e. Array (Cause e) -> Maybe (Cause e)
combineParallel arr = case Array.uncons arr of
  Nothing -> Nothing
  Just { head, tail } -> Just (Array.foldl Parallel head tail)

-- | All typed failures (`Fail` leaves) in left-to-right order.
-- |
-- | A cause built from a pure success or a defect-only path returns
-- | an empty array. Composite causes (Parallel / Sequential) are
-- | flattened depth-first, left branch first.
failures :: forall e. Cause e -> Array (Variant e)
failures = case _ of
  Fail v -> [ v ]
  Die _ -> []
  Parallel a b -> failures a <> failures b
  Sequential a b -> failures a <> failures b

-- | All defects (`Die` leaves) in left-to-right order.
defects :: forall e. Cause e -> Array Error
defects = case _ of
  Fail _ -> []
  Die err -> [ err ]
  Parallel a b -> defects a <> defects b
  Sequential a b -> defects a <> defects b

-- | At least one typed failure somewhere in the cause tree?
hasFailure :: forall e. Cause e -> Boolean
hasFailure = case _ of
  Fail _ -> true
  Die _ -> false
  Parallel a b -> hasFailure a || hasFailure b
  Sequential a b -> hasFailure a || hasFailure b

-- | At least one defect somewhere in the cause tree?
hasDefect :: forall e. Cause e -> Boolean
hasDefect = case _ of
  Fail _ -> false
  Die _ -> true
  Parallel a b -> hasDefect a || hasDefect b
  Sequential a b -> hasDefect a || hasDefect b

-- | True if the cause is *purely* typed failures (no defects).
-- |
-- | Useful as a quick gate before a `failCause` rethrow or a
-- | recovery path that only knows how to handle the typed row.
isFailure :: forall e. Cause e -> Boolean
isFailure c = hasFailure c && not (hasDefect c)

-- | Does any typed failure inside the cause satisfy the predicate?
-- |
-- | Defects are skipped; only `Fail` leaves are inspected.
containsFailure :: forall e. (Variant e -> Boolean) -> Cause e -> Boolean
containsFailure p = case _ of
  Fail v -> p v
  Die _ -> false
  Parallel a b -> containsFailure p a || containsFailure p b
  Sequential a b -> containsFailure p a || containsFailure p b

-- | Fold a cause tree into a value. The four arguments handle the
-- | four constructors: a typed-failure leaf, a defect leaf, a
-- | parallel composite, and a sequential composite. The recursion
-- | is bottom-up: composite branches are folded first and the
-- | combiner is called with their already-folded values.
-- |
-- | This is the catamorphism for `Cause`; every other inspection
-- | helper (`failures`, `defects`, `hasFailure`, etc.) can be
-- | written in terms of it. Reach for it when you need a custom
-- | aggregation that does not match the existing helpers, e.g. a
-- | "first error in a Sequential chain" reducer or a structured
-- | JSON render.
foldCause
  :: forall e b
   . (Variant e -> b)
  -> (Error -> b)
  -> (b -> b -> b)
  -> (b -> b -> b)
  -> Cause e
  -> b
foldCause onFail onDie onPar onSeq = go
  where
  go = case _ of
    Fail v -> onFail v
    Die err -> onDie err
    Parallel a b -> onPar (go a) (go b)
    Sequential a b -> onSeq (go a) (go b)

-- | Flatten the cause tree into a left-to-right array of leaves,
-- | discarding the parallel-vs-sequential structure. `Right` carries
-- | a typed failure, `Left` a defect.
-- |
-- | Pair with `prettyCause` when you only need a flat list of
-- | leaves and not the tree shape (e.g. for a one-line log or a
-- | metric counter keyed by failure tag).
linearize :: forall e. Cause e -> Array (Either Error (Variant e))
linearize = foldCause
  (\v -> [ Right v ])
  (\err -> [ Left err ])
  (\a b -> a <> b)
  (\a b -> a <> b)

-- | Search the cause tree for the first node where the projection
-- | returns `Just`. Visits every node (atomic and composite); the
-- | first `Just` wins, in depth-first left-to-right order.
-- |
-- | The projection sees the full `Cause e` node, so callers can
-- | match on subtrees as well as leaves.
find :: forall e b. (Cause e -> Maybe b) -> Cause e -> Maybe b
find p = go
  where
  go c = case p c of
    Just b -> Just b
    Nothing -> case c of
      Fail _ -> Nothing
      Die _ -> Nothing
      Parallel a b -> case go a of
        Just r -> Just r
        Nothing -> go b
      Sequential a b -> case go a of
        Just r -> Just r
        Nothing -> go b

-- | Render a `Cause` as a multi-line, human-readable tree.
-- |
-- | The caller supplies a renderer for typed failures (`Variant e
-- | -> String`) because `Variant` does not have a generic `Show`.
-- | Defects are rendered via `Effect.Exception.message`; if you
-- | also want the JS stack underneath the message, reach for
-- | `prettyCauseWithStack` instead.
-- |
-- | The output format:
-- |
-- |   * Atomic causes render on a single line.
-- |   * `Parallel` introduces a header and indents both branches.
-- |   * `Sequential` does the same, with a different header so
-- |     readers can tell the two apart.
prettyCause :: forall e. (Variant e -> String) -> Cause e -> String
prettyCause renderFail =
  prettyCauseGo renderFail dieMessageOnly 0

-- | Like `prettyCause`, but each `Die` leaf renders the JS stack
-- | trace underneath its message (when one is available). Each
-- | stack line is indented one level deeper than the `defect:`
-- | header so the tree structure stays readable.
-- |
-- | This is the renderer to reach for when a defect is going to a
-- | log file or a bug report and the developer wants to know where
-- | the underlying `Aff` exception originated.
prettyCauseWithStack
  :: forall e. (Variant e -> String) -> Cause e -> String
prettyCauseWithStack renderFail =
  prettyCauseGo renderFail dieMessageWithStack 0

prettyCauseGo
  :: forall e
   . (Variant e -> String)
  -> (Int -> Error -> String)
  -> Int
  -> Cause e
  -> String
prettyCauseGo renderFail renderDie = go
  where
  go depth = case _ of
    Fail v -> pad depth <> "fail: " <> renderFail v
    Die err -> renderDie depth err
    Parallel a b ->
      pad depth <> "parallel failures:\n"
        <> Array.intercalate "\n"
          [ go (depth + 1) a, go (depth + 1) b ]
    Sequential a b ->
      pad depth <> "sequenced failures:\n"
        <> Array.intercalate "\n"
          [ go (depth + 1) a, go (depth + 1) b ]

dieMessageOnly :: Int -> Error -> String
dieMessageOnly depth err = pad depth <> "defect: " <> message err

dieMessageWithStack :: Int -> Error -> String
dieMessageWithStack depth err = case stack err of
  Nothing -> dieMessageOnly depth err
  Just s ->
    pad depth <> "defect: " <> message err <> "\n"
      <> indentLines (depth + 1) s

-- | Split a multi-line string and re-emit each non-empty line
-- | prefixed with `pad depth`. Empty lines (e.g. a trailing blank
-- | from the JS stack) are dropped to keep the output compact.
indentLines :: Int -> String -> String
indentLines depth s =
  let
    lines = String.split (String.Pattern "\n") s
    kept = Array.mapMaybe
      ( \line ->
          if line == "" then Nothing
          else Just (pad depth <> line)
      )
      lines
  in
    String.joinWith "\n" kept

pad :: Int -> String
pad n = repeatStr n "  "

-- A tiny utility kept private so the module stays self-contained.
repeatStr :: Int -> String -> String
repeatStr n s
  | n <= 0 = ""
  | otherwise = s <> repeatStr (n - 1) s
