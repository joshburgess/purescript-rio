-- | Introspection for fibers.
-- |
-- | Each fiber carries an auto-assigned numeric `FiberId` and an
-- | optional human label. The label is mutable and defaults to
-- | `Nothing`; callers can set it from inside the running fiber via
-- | `setLabel`, or against a fiber handle via `setFiberLabel`. Labels
-- | are purely diagnostic: nothing in the runtime branches on them.
-- |
-- | `dump` returns a snapshot of a fiber's identity and current
-- | status. Use it from supervisors, logging, or test assertions that
-- | need to identify which fiber produced an event.
module RIO.Fiber.Inspect
  ( module FiberIdExports
  , FiberStatus(..)
  , FiberDump
  , currentFiberId
  , currentLabel
  , setLabel
  , fiberId
  , fiberLabel
  , setFiberLabel
  , fiberStatus
  , dump
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import RIO.Fiber.FiberId (FiberId(..), unFiberId) as FiberIdExports
import RIO.Fiber.FiberId (FiberId(..))
import RIO.Fiber.Internal (Fiber, RIO(..))
import RIO.Fiber.Internal as Internal

-- | Scheduler-visible state of a fiber.
data FiberStatus
  = Running
  | Suspended
  | Done

derive instance eqFiberStatus :: Eq FiberStatus

instance showFiberStatus :: Show FiberStatus where
  show Running = "Running"
  show Suspended = "Suspended"
  show Done = "Done"

statusFromCode :: Int -> FiberStatus
statusFromCode 0 = Running
statusFromCode 1 = Suspended
statusFromCode _ = Done

-- | A snapshot of a fiber's identity and current scheduler state.
type FiberDump =
  { id :: FiberId
  , label :: Maybe String
  , status :: FiberStatus
  }

-- | Read the current fiber's id.
currentFiberId :: forall r e. RIO r e FiberId
currentFiberId = RIO (Internal.opMap FiberId Internal.opCurrentFiberId)

-- | Read the current fiber's label, if one has been set.
currentLabel :: forall r e. RIO r e (Maybe String)
currentLabel = RIO (Internal.opMap Internal.nullableLabelToMaybe Internal.opCurrentFiberLabel)

-- | Attach (or replace) the current fiber's label. Labels are purely
-- | diagnostic and do not affect scheduling.
setLabel :: forall r e. String -> RIO r e Unit
setLabel label = RIO (Internal.opSetCurrentFiberLabel label)

-- | Read a fiber handle's id. Pure: the id never changes.
fiberId :: forall e a. Fiber e a -> FiberId
fiberId f = FiberId (Internal._fiberId f)

-- | Read a fiber handle's label, if one has been set.
fiberLabel :: forall e a. Fiber e a -> Effect (Maybe String)
fiberLabel f = map Internal.nullableLabelToMaybe (Internal._fiberLabel f)

-- | Attach (or replace) a fiber handle's label from outside the fiber.
setFiberLabel :: forall e a. Fiber e a -> String -> Effect Unit
setFiberLabel = Internal._fiberSetLabel

-- | Read a fiber handle's current scheduler state.
fiberStatus :: forall e a. Fiber e a -> Effect FiberStatus
fiberStatus f = map statusFromCode (Internal._fiberStatusCode f)

-- | Take a snapshot of a fiber's identity and current state. Cheap
-- | enough to call from supervisors or log lines.
dump :: forall e a. Fiber e a -> Effect FiberDump
dump f = do
  label <- fiberLabel f
  status <- fiberStatus f
  pure { id: fiberId f, label, status }
