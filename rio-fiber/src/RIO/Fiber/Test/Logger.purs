-- | An in-memory `Logger` backend for tests.
-- |
-- | `newRecordingLogger` returns the service plus a `snapshot`
-- | action that returns every emission in order. Each record
-- | carries the level and the rendered message exactly as the
-- | logger received it.
-- |
-- | `RIO.Fiber.Logger.annotateLogs` works by wrapping the active
-- | `Logger` so it prefixes a rendered key/value string into the
-- | message before calling `emit`. The recording backend sees the
-- | already-prefixed string; assert on it directly if you want to
-- | check that annotations propagated.
module RIO.Fiber.Test.Logger
  ( LogRecord
  , RecordingLogger
  , newRecordingLogger
  ) where

import Prelude

import Data.Array (snoc) as Array
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Fiber.Logger (LogLevel, Logger(..))

-- | One captured emission.
type LogRecord =
  { level :: LogLevel
  , message :: String
  }

-- | A `Logger` service paired with a snapshot reader.
type RecordingLogger =
  { logger :: Logger
  , snapshot :: Effect (Array LogRecord)
  }

-- | Allocate a fresh recording backend.
newRecordingLogger :: Effect RecordingLogger
newRecordingLogger = do
  recordsRef <- Ref.new ([] :: Array LogRecord)
  let
    logger :: Logger
    logger = Logger
      { emit: \level message ->
          Ref.modify_
            (\xs -> Array.snoc xs { level, message })
            recordsRef
      }

    snapshot :: Effect (Array LogRecord)
    snapshot = Ref.read recordsRef
  pure { logger, snapshot }
