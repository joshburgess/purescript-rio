-- | An in-memory `Logger` backend for tests.
-- |
-- | `newRecordingLogger` returns the service plus a `snapshot`
-- | action that returns every emission in order. Each record
-- | carries the level, the message, and the merged annotation
-- | set that was current at emission time, so tests can assert
-- | both on what was logged and on which `withFields` blocks
-- | were active.
module RIO.Test.Logger
  ( LogRecord
  , RecordingLogger
  , newRecordingLogger
  ) where

import Prelude

import Data.Array (snoc) as Array
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Logger (LogLevel, Logger)

-- | One captured emission. `fields` is the merged annotation
-- | set at the time `log` was called (i.e. the cumulative
-- | result of every active `withFields` block).
type LogRecord =
  { level :: LogLevel
  , message :: String
  , fields :: Array (Tuple String String)
  }

-- | A `Logger` service paired with a snapshot reader.
type RecordingLogger =
  { logger :: Logger
  , snapshot :: Effect (Array LogRecord)
  }

-- | Allocate a fresh recording backend.
newRecordingLogger :: Aff RecordingLogger
newRecordingLogger = liftEffect do
  recordsRef <- Ref.new ([] :: Array LogRecord)
  annotationsRef <- Ref.new ([] :: Array (Tuple String String))
  let
    onLog level message fields =
      Ref.modify_
        (\xs -> Array.snoc xs { level, message, fields })
        recordsRef

    logger :: Logger
    logger =
      { log: onLog
      , getAnnotations: Ref.read annotationsRef
      , setAnnotations: \as -> Ref.write as annotationsRef
      }

    snapshot :: Effect (Array LogRecord)
    snapshot = Ref.read recordsRef
  pure { logger, snapshot }
