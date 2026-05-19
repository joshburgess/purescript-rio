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
  ) where

import Prelude

import Data.Variant (Variant)
import Effect.Exception (Error)

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
