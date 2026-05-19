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
  , hasDefect
  , failures
  , defects
  , firstFailure
  , firstDefect
  , interruptCount
  , stripInterrupts
  , stripFailures
  , stripDefects
  , mapFailures
  , flatten
  , squash
  , fold
  , prettyPrint
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Variant (Variant)
import Effect.Exception (Error, error, message)

-- | A structured description of how a fiber failed.
data Cause e
  = Empty
  | Fail (Variant e)
  | Die Error
  | Interrupt
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

-- | The interrupt cause.
interrupt :: forall e. Cause e
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
isInterrupted Interrupt = true
isInterrupted (Then a b) = isInterrupted a || isInterrupted b
isInterrupted (Both a b) = isInterrupted a || isInterrupted b
isInterrupted _ = false

-- | Does this cause contain a defect anywhere?
hasDefect :: forall e. Cause e -> Boolean
hasDefect (Die _) = true
hasDefect (Then a b) = hasDefect a || hasDefect b
hasDefect (Both a b) = hasDefect a || hasDefect b
hasDefect _ = false

-- | All typed failures in left-to-right order.
failures :: forall e. Cause e -> Array (Variant e)
failures c = go c []
  where
  go Empty acc = acc
  go (Fail v) acc = acc <> [ v ]
  go (Die _) acc = acc
  go Interrupt acc = acc
  go (Then a b) acc = go b (go a acc)
  go (Both a b) acc = go b (go a acc)

-- | All defects in left-to-right order.
defects :: forall e. Cause e -> Array Error
defects c = go c []
  where
  go Empty acc = acc
  go (Fail _) acc = acc
  go (Die e) acc = acc <> [ e ]
  go Interrupt acc = acc
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
interruptCount Interrupt = 1
interruptCount (Then a b) = interruptCount a + interruptCount b
interruptCount (Both a b) = interruptCount a + interruptCount b
interruptCount _ = 0

-- | Replace every `Interrupt` leaf with `Empty` and re-collapse, so
-- | pure-interrupt subtrees disappear. Useful before logging when
-- | interrupts are expected and would be noise.
stripInterrupts :: forall e. Cause e -> Cause e
stripInterrupts Empty = Empty
stripInterrupts (Fail v) = Fail v
stripInterrupts (Die e) = Die e
stripInterrupts Interrupt = Empty
stripInterrupts (Then a b) = then_ (stripInterrupts a) (stripInterrupts b)
stripInterrupts (Both a b) = both (stripInterrupts a) (stripInterrupts b)

-- | Replace every typed failure with `Empty` and re-collapse, keeping
-- | only defects and interrupts.
stripFailures :: forall e. Cause e -> Cause e
stripFailures Empty = Empty
stripFailures (Fail _) = Empty
stripFailures (Die e) = Die e
stripFailures Interrupt = Interrupt
stripFailures (Then a b) = then_ (stripFailures a) (stripFailures b)
stripFailures (Both a b) = both (stripFailures a) (stripFailures b)

-- | Replace every defect with `Empty` and re-collapse, keeping only
-- | typed failures and interrupts.
stripDefects :: forall e. Cause e -> Cause e
stripDefects Empty = Empty
stripDefects (Fail v) = Fail v
stripDefects (Die _) = Empty
stripDefects Interrupt = Interrupt
stripDefects (Then a b) = then_ (stripDefects a) (stripDefects b)
stripDefects (Both a b) = both (stripDefects a) (stripDefects b)

-- | Map every typed failure to a different error row, preserving
-- | tree shape. Defects and interrupts are unchanged.
mapFailures :: forall e e'. (Variant e -> Variant e') -> Cause e -> Cause e'
mapFailures _ Empty = Empty
mapFailures f (Fail v) = Fail (f v)
mapFailures _ (Die e) = Die e
mapFailures _ Interrupt = Interrupt
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
     , interrupt :: r
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
  go Interrupt = k.interrupt
  go (Then a b) = k.then_ (go a) (go b)
  go (Both a b) = k.both (go a) (go b)

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
  go Interrupt = [ "Interrupt" ]
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
