-- | A small pull-based chunk stream used for streaming HTTP
-- | request and response bodies. Each `BodyStream` is an `Effect`
-- | computation that returns the next chunk or `Nothing` when the
-- | stream has been fully consumed.
-- |
-- | The shape is intentionally `Effect`-typed (not `RIO`-typed) so
-- | a body stream can be embedded into any `RIO r e` context via
-- | `liftEffect`. Producers that want to draw from `RIO` services
-- | should perform the relevant work up front, snapshot the data
-- | into `Ref`s, and return an Effect-typed pull function that
-- | walks the snapshot.
-- |
-- | A `BodyStream` is single-use: each call to the pull function
-- | yields the next chunk and advances the internal cursor. Pull
-- | functions are expected to be referentially well-behaved
-- | (calling them after `Nothing` keeps returning `Nothing`) but
-- | otherwise carry whatever side effects the producer needs to
-- | unfold their source.
module RIO.Fiber.HttpStream
  ( BodyStream
  , fromString
  , fromChunks
  , empty
  , drain
  , drainTo
  , takeChunks
  , map
  , chunkSize
  ) where

import Prelude hiding (map)
import Prelude as P

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Ref as Ref

-- | A pull-based chunk stream. Each call to the wrapped `Effect`
-- | returns the next chunk or `Nothing` when the stream has been
-- | fully consumed.
type BodyStream = Effect (Maybe String)

-- | An empty stream. Always returns `Nothing`.
empty :: BodyStream
empty = pure Nothing

-- | A stream of a single string, delivered in one chunk.
fromString :: String -> Effect BodyStream
fromString s = do
  ref <- Ref.new false
  pure do
    delivered <- Ref.read ref
    if delivered then pure Nothing
    else do
      Ref.write true ref
      pure (Just s)

-- | A stream that delivers each chunk of the input array in
-- | order. Empty chunks are preserved as-is (the stream returns
-- | them rather than skipping); callers that want to coalesce
-- | them should filter the input.
fromChunks :: Array String -> Effect BodyStream
fromChunks chunks = do
  ref <- Ref.new 0
  pure do
    ix <- Ref.read ref
    case Array.index chunks ix of
      Nothing -> pure Nothing
      Just c -> do
        Ref.write (ix + 1) ref
        pure (Just c)

-- | Drain a `BodyStream` into a single concatenated string.
drain :: BodyStream -> Effect String
drain stream = go ""
  where
  go acc = do
    next <- stream
    case next of
      Nothing -> pure acc
      Just c -> go (acc <> c)

-- | Drain a `BodyStream` chunk-by-chunk into a consumer
-- | action. The consumer is called once per chunk in order;
-- | when the stream ends, `drainTo` returns.
drainTo :: forall a. (String -> Effect a) -> BodyStream -> Effect Unit
drainTo consumer stream = go
  where
  go = do
    next <- stream
    case next of
      Nothing -> pure unit
      Just c -> do
        _ <- consumer c
        go

-- | Read up to `n` chunks from a stream. Returns the chunks
-- | read and the remainder of the stream; useful when the
-- | caller wants to peek at a header chunk before deciding how
-- | to consume the rest.
takeChunks
  :: Int
  -> BodyStream
  -> Effect { chunks :: Array String, rest :: BodyStream }
takeChunks limit stream = go 0 []
  where
  go i acc
    | i >= limit = pure { chunks: acc, rest: stream }
    | otherwise = do
        next <- stream
        case next of
          Nothing -> pure { chunks: acc, rest: empty }
          Just c -> go (i + 1) (acc <> [ c ])

-- | Transform each chunk as it flows through the stream.
map :: (String -> String) -> BodyStream -> BodyStream
map f stream = do
  next <- stream
  pure (P.map f next)

-- | The byte length (UTF-16 code units, matching `Data.String`)
-- | of all chunks in the stream once drained. Reads the stream
-- | to completion, so it cannot be used to inspect a stream
-- | non-destructively; pair with `fromString (acc)` if the
-- | caller needs both the size and the body.
chunkSize :: BodyStream -> Effect Int
chunkSize stream = do
  s <- drain stream
  pure (String.length s)
