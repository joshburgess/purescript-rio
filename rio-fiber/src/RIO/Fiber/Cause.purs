-- | Cause: a structured description of why a fiber stopped running.
-- |
-- | A `Cause e` records every reason a computation failed: zero or
-- | more typed failures (`Fail`), zero or more JS defects (`Die`),
-- | and zero or more interrupts. `Then` composes causes that arose
-- | sequentially (action failed, then finalizer also failed);
-- | `Both` composes causes that arose in parallel (two race losers,
-- | two fork-join branches).
-- |
-- | The interpreter composes causes at finalizer boundaries: a
-- | failing finalizer after a failing action produces `Then action fin`
-- | rather than overwriting one with the other. Users see the
-- | composed cause through `causeOf`, or can raise one directly with
-- | `failCause`. Parallel composition (`Both`) is reserved for future
-- | `race` / `parTraverse` use cases that report multiple branches.
module RIO.Fiber.Cause
  ( Cause(..)
  , empty
  , fail
  , die
  , interrupt
  , then_
  , both
  , isEmpty
  , isInterrupted
  , isInterruptedOnly
  , hasDefect
  , hasFailure
  , isFailure
  , failures
  , defects
  , firstFailure
  , firstDefect
  , interruptCount
  , interrupters
  , stripInterrupts
  , stripFailures
  , stripDefects
  , mapFailures
  , flatten
  , squash
  , fold
  , linearize
  , prettyPrint
  , prettyCause
  , find
  , contains
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Variant (Variant)
import Effect.Exception (Error, error, message)
import RIO.Fiber.FiberId (FiberId, unFiberId)

-- | A structured description of how a fiber failed.
-- |
-- | `Interrupt` carries the `FiberId` of the fiber that issued the
-- | interrupt. Interrupts that originate outside any running fiber
-- | (e.g. an external `Fiber.interrupt` call) are tagged with the
-- | sentinel `externalFiberId`.
data Cause e
  = Empty
  | Fail (Variant e)
  | Die Error
  | Interrupt FiberId
  | Then (Cause e) (Cause e)
  | Both (Cause e) (Cause e)

-- | The empty cause: "no failure observed".
empty :: forall e. Cause e
empty = Empty

-- | Wrap a typed failure into a cause.
fail :: forall e. Variant e -> Cause e
fail = Fail

-- | Wrap a defect (JS error) into a cause.
die :: forall e. Error -> Cause e
die = Die

-- | Build an interrupt cause attributed to the given fiber.
interrupt :: forall e. FiberId -> Cause e
interrupt = Interrupt

-- | Sequential composition; collapses `Empty` on either side.
then_ :: forall e. Cause e -> Cause e -> Cause e
then_ Empty c = c
then_ c Empty = c
then_ a b = Then a b

-- | Parallel composition; collapses `Empty` on either side.
both :: forall e. Cause e -> Cause e -> Cause e
both Empty c = c
both c Empty = c
both a b = Both a b

-- | Is this cause empty all the way down?
isEmpty :: forall e. Cause e -> Boolean
isEmpty Empty = true
isEmpty (Then a b) = isEmpty a && isEmpty b
isEmpty (Both a b) = isEmpty a && isEmpty b
isEmpty _ = false

-- | Does this cause contain an interrupt anywhere?
isInterrupted :: forall e. Cause e -> Boolean
isInterrupted (Interrupt _) = true
isInterrupted (Then a b) = isInterrupted a || isInterrupted b
isInterrupted (Both a b) = isInterrupted a || isInterrupted b
isInterrupted _ = false

-- | Is every non-`Empty` leaf in this cause an `Interrupt`? Returns
-- | `false` when any `Fail` or `Die` leaf is present, and `false`
-- | for a fully-empty cause (which has no observed outcome at all).
-- |
-- | Use this to distinguish "I was cancelled, nothing failed" from
-- | "I was cancelled while a failure was already unwinding" — common
-- | branch in shutdown logic where the latter is reportable but the
-- | former is expected.
isInterruptedOnly :: forall e. Cause e -> Boolean
isInterruptedOnly c = isInterrupted c && not (hasDefect c) && Array.null (failures c)

-- | Does this cause contain a defect anywhere?
hasDefect :: forall e. Cause e -> Boolean
hasDefect (Die _) = true
hasDefect (Then a b) = hasDefect a || hasDefect b
hasDefect (Both a b) = hasDefect a || hasDefect b
hasDefect _ = false

-- | At least one typed `Fail` somewhere in the cause tree?
hasFailure :: forall e. Cause e -> Boolean
hasFailure (Fail _) = true
hasFailure (Then a b) = hasFailure a || hasFailure b
hasFailure (Both a b) = hasFailure a || hasFailure b
hasFailure _ = false

-- | The cause is composed purely of typed `Fail` leaves: at least
-- | one failure, no defects, no interrupts. Useful as a gate before
-- | a recovery path that only handles the typed row (e.g. before
-- | `failCause` to rethrow, or before destructuring `failures`).
isFailure :: forall e. Cause e -> Boolean
isFailure c = hasFailure c && not (hasDefect c) && not (isInterrupted c)

-- | All typed failures in left-to-right order.
failures :: forall e. Cause e -> Array (Variant e)
failures c = go c []
  where
  go Empty acc = acc
  go (Fail v) acc = acc <> [ v ]
  go (Die _) acc = acc
  go (Interrupt _) acc = acc
  go (Then a b) acc = go b (go a acc)
  go (Both a b) acc = go b (go a acc)

-- | All defects in left-to-right order.
defects :: forall e. Cause e -> Array Error
defects c = go c []
  where
  go Empty acc = acc
  go (Fail _) acc = acc
  go (Die e) acc = acc <> [ e ]
  go (Interrupt _) acc = acc
  go (Then a b) acc = go b (go a acc)
  go (Both a b) acc = go b (go a acc)

-- | The leftmost typed failure, or `Nothing` if none.
firstFailure :: forall e. Cause e -> Maybe (Variant e)
firstFailure (Fail v) = Just v
firstFailure (Then a b) = case firstFailure a of
  Just v -> Just v
  Nothing -> firstFailure b
firstFailure (Both a b) = case firstFailure a of
  Just v -> Just v
  Nothing -> firstFailure b
firstFailure _ = Nothing

-- | The leftmost defect, or `Nothing` if none.
firstDefect :: forall e. Cause e -> Maybe Error
firstDefect (Die e) = Just e
firstDefect (Then a b) = case firstDefect a of
  Just e -> Just e
  Nothing -> firstDefect b
firstDefect (Both a b) = case firstDefect a of
  Just e -> Just e
  Nothing -> firstDefect b
firstDefect _ = Nothing

-- | Count of `Interrupt` leaves anywhere in the tree.
interruptCount :: forall e. Cause e -> Int
interruptCount (Interrupt _) = 1
interruptCount (Then a b) = interruptCount a + interruptCount b
interruptCount (Both a b) = interruptCount a + interruptCount b
interruptCount _ = 0

-- | All interrupter ids in left-to-right order. An empty array means
-- | the cause carries no interrupts. Useful for attribution: pick the
-- | first id to blame the upstream interrupter, or fold the array to
-- | spot races where multiple fibers interrupted concurrently.
interrupters :: forall e. Cause e -> Array FiberId
interrupters c = go c []
  where
  go Empty acc = acc
  go (Fail _) acc = acc
  go (Die _) acc = acc
  go (Interrupt fid) acc = acc <> [ fid ]
  go (Then a b) acc = go b (go a acc)
  go (Both a b) acc = go b (go a acc)

-- | Replace every `Interrupt` leaf with `Empty` and re-collapse, so
-- | pure-interrupt subtrees disappear. Useful before logging when
-- | interrupts are expected and would be noise.
stripInterrupts :: forall e. Cause e -> Cause e
stripInterrupts Empty = Empty
stripInterrupts (Fail v) = Fail v
stripInterrupts (Die e) = Die e
stripInterrupts (Interrupt _) = Empty
stripInterrupts (Then a b) = then_ (stripInterrupts a) (stripInterrupts b)
stripInterrupts (Both a b) = both (stripInterrupts a) (stripInterrupts b)

-- | Replace every typed failure with `Empty` and re-collapse, keeping
-- | only defects and interrupts.
stripFailures :: forall e. Cause e -> Cause e
stripFailures Empty = Empty
stripFailures (Fail _) = Empty
stripFailures (Die e) = Die e
stripFailures (Interrupt fid) = Interrupt fid
stripFailures (Then a b) = then_ (stripFailures a) (stripFailures b)
stripFailures (Both a b) = both (stripFailures a) (stripFailures b)

-- | Replace every defect with `Empty` and re-collapse, keeping only
-- | typed failures and interrupts.
stripDefects :: forall e. Cause e -> Cause e
stripDefects Empty = Empty
stripDefects (Fail v) = Fail v
stripDefects (Die _) = Empty
stripDefects (Interrupt fid) = Interrupt fid
stripDefects (Then a b) = then_ (stripDefects a) (stripDefects b)
stripDefects (Both a b) = both (stripDefects a) (stripDefects b)

-- | Map every typed failure to a different error row, preserving
-- | tree shape. Defects and interrupts are unchanged.
mapFailures :: forall e e'. (Variant e -> Variant e') -> Cause e -> Cause e'
mapFailures _ Empty = Empty
mapFailures f (Fail v) = Fail (f v)
mapFailures _ (Die e) = Die e
mapFailures _ (Interrupt fid) = Interrupt fid
mapFailures f (Then a b) = Then (mapFailures f a) (mapFailures f b)
mapFailures f (Both a b) = Both (mapFailures f a) (mapFailures f b)

-- | Collapse a cause to its three leaf populations.
flatten
  :: forall e
   . Cause e
  -> { failures :: Array (Variant e)
     , defects :: Array Error
     , interrupted :: Boolean
     }
flatten c =
  { failures: failures c
  , defects: defects c
  , interrupted: isInterrupted c
  }

-- | Squash a cause into a single `Error` for handoff to code that
-- | does not understand `Cause`. Priority: first defect, then first
-- | typed failure (rendered through the user-supplied projector),
-- | then an interrupt placeholder, then an empty-cause placeholder.
squash :: forall e. (Variant e -> Error) -> Cause e -> Error
squash render c = case firstDefect c of
  Just e -> e
  Nothing -> case firstFailure c of
    Just v -> render v
    Nothing ->
      if isInterrupted c then error "rio-fiber: cause squashed from interrupt"
      else error "rio-fiber: empty cause squashed"

-- | Universal fold over `Cause`. Provide one case per constructor;
-- | every other helper here can be expressed in terms of `fold`.
fold
  :: forall e r
   . { empty :: r
     , fail :: Variant e -> r
     , die :: Error -> r
     , interrupt :: FiberId -> r
     , then_ :: r -> r -> r
     , both :: r -> r -> r
     }
  -> Cause e
  -> r
fold k = go
  where
  go Empty = k.empty
  go (Fail v) = k.fail v
  go (Die e) = k.die e
  go (Interrupt fid) = k.interrupt fid
  go (Then a b) = k.then_ (go a) (go b)
  go (Both a b) = k.both (go a) (go b)

-- | Flatten the cause tree into a left-to-right array of leaves,
-- | discarding the `Then` vs `Both` structure and the `Empty` /
-- | `Interrupt` markers. `Right` carries a typed failure, `Left` a
-- | defect.
-- |
-- | Pair with `prettyPrint` when you only need a flat list of
-- | leaves and not the tree shape (e.g. for a one-line log or a
-- | metric counter keyed by failure tag).
linearize :: forall e. Cause e -> Array (Either Error (Variant e))
linearize = fold
  { empty: []
  , fail: \v -> [ Right v ]
  , die: \err -> [ Left err ]
  , interrupt: \_ -> []
  , then_: append
  , both: append
  }

-- | Render a `Cause` as an indented multi-line tree. The caller
-- | supplies a printer for the typed-failure payload (typically built
-- | from `Variant.match`). Defects are rendered via `Exception.message`.
prettyPrint :: forall e. (Variant e -> String) -> Cause e -> String
prettyPrint render = joinWith "\n" <<< go
  where
  go :: Cause e -> Array String
  go Empty = [ "Empty" ]
  go (Fail v) = [ "Fail " <> render v ]
  go (Die e) = [ "Die " <> message e ]
  go (Interrupt fid) = [ "Interrupt by #" <> show (unFiberId fid) ]
  go (Then a b) = [ "Then" ] <> branch false (go a) <> branch true (go b)
  go (Both a b) = [ "Both" ] <> branch false (go a) <> branch true (go b)

  branch :: Boolean -> Array String -> Array String
  branch isLast xs = case Array.uncons xs of
    Nothing -> []
    Just { head, tail } ->
      let
        firstPrefix = if isLast then "`-- " else "|-- "
        contPrefix = if isLast then "    " else "|   "
      in
        [ firstPrefix <> head ] <> map (contPrefix <> _) tail

-- | Alias for `prettyPrint`, named to mirror rio-aff.
prettyCause :: forall e. (Variant e -> String) -> Cause e -> String
prettyCause = prettyPrint

-- | Find the first sub-cause that matches the predicate, walking
-- | the tree left-to-right. Returns `Nothing` when no node matches.
-- | The predicate sees each sub-tree node, not just leaves: so
-- | `find isInterrupted (Then ...)` may return the composite if it
-- | contains an interrupt anywhere.
find :: forall e. (Cause e -> Boolean) -> Cause e -> Maybe (Cause e)
find p = go
  where
  go c
    | p c = Just c
    | otherwise = case c of
        Then a b -> case go a of
          Just hit -> Just hit
          Nothing -> go b
        Both a b -> case go a of
          Just hit -> Just hit
          Nothing -> go b
        _ -> Nothing

-- | `true` iff some sub-cause matches the predicate.
contains :: forall e. (Cause e -> Boolean) -> Cause e -> Boolean
contains p c = case find p c of
  Just _ -> true
  Nothing -> false
