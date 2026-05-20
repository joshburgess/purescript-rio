-- | RIO-flavoured wrappers around `Node.Stream` and
-- | `Node.Stream.Aff`.
-- |
-- | A `Stream rw` is a value (a handle on a node-level readable,
-- | writable, or duplex stream), not a capability, so this module
-- | mirrors the upstream surface by lifting each `Effect` / `Aff`
-- | operation into `RIO` and re-exporting the types and event
-- | handles unchanged.
-- |
-- | The Node.js stream API has two writers and two ends — the
-- | direct `Effect`-returning forms from `Node.Stream` (which give
-- | back a `Boolean` indicating whether the chunk was flushed or
-- | will require waiting on `drainH`) and the backpressure-aware
-- | `Aff`-blocking forms from `Node.Stream.Aff` (which wait for
-- | `drainH` / `finishH` themselves). The Effect-style writers keep
-- | their original names (`write`, `writeString`, `end`); the
-- | blocking forms are surfaced as `writeAll` (writes an
-- | `Array Buffer`, awaiting `drainH` between chunks) and
-- | `endAwait` (resolves once `finishH` has fired).
module RIO.Aff.Node.Stream
  ( module Exports
  , allowHalfOpen
  , closed
  , cork
  , destroy
  , destroy'
  , destroyed
  , end
  , end'
  , endAwait
  , errored
  , fromStringUTF8
  , isPaused
  , newPassThrough
  , pause
  , pipe
  , pipe'
  , pipeline
  , read
  , read'
  , readAll
  , readEither
  , readEither'
  , readN
  , readSome
  , readString
  , readString'
  , readable
  , readableEnded
  , readableFlowing
  , readableFromBuffer
  , readableFromString
  , readableHighWaterMark
  , readableLength
  , readableToBuffers
  , readableToString
  , readableToStringUtf8
  , resume
  , setDefaultEncoding
  , setEncoding
  , toStringUTF8
  , uncork
  , unpipe
  , unpipeAll
  , write
  , write'
  , writeAll
  , writeString
  , writeString'
  , writeable
  , writeableCorked
  , writeableEnded
  , writeableFinished
  , writeableHighWaterMark
  , writeableLength
  , writeableNeedDrain
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream
  ( Chunk
  , Duplex
  , Read
  , Readable
  , Stream
  , Writable
  , Write
  , closeH
  , dataH
  , dataHEither
  , dataHStr
  , drainH
  , endH
  , errorH
  , finishH
  , pauseH
  , pipeH
  , readableH
  , resumeH
  , toEventEmitter
  , unpipeH
  ) as Exports
import Node.Stream (Duplex, Readable, Stream, Writable)
import Node.Stream as NS
import Node.Stream.Aff as NSAff

import RIO.Aff.Core (RIO)

-- | Whether the readable side is currently producing data.
readable :: forall r e w. Readable w -> RIO r e Boolean
readable r = liftEffect (NS.readable r)

-- | Whether the readable side has signalled end-of-stream.
readableEnded :: forall r e w. Readable w -> RIO r e Boolean
readableEnded r = liftEffect (NS.readableEnded r)

-- | Whether the readable side is in flowing mode.
readableFlowing :: forall r e w. Readable w -> RIO r e Boolean
readableFlowing r = liftEffect (NS.readableFlowing r)

-- | The high-water mark configured for the readable side.
readableHighWaterMark :: forall r e w. Readable w -> RIO r e Boolean
readableHighWaterMark r = liftEffect (NS.readableHighWaterMark r)

-- | The number of bytes (or objects, for an object-mode stream)
-- | currently buffered on the readable side.
readableLength :: forall r e w. Readable w -> RIO r e Boolean
readableLength r = liftEffect (NS.readableLength r)

-- | Resume reading; transitions the stream into flowing mode.
resume :: forall r e w. Readable w -> RIO r e Unit
resume r = liftEffect (NS.resume r)

-- | Pause reading; transitions the stream out of flowing mode.
pause :: forall r e w. Readable w -> RIO r e Unit
pause r = liftEffect (NS.pause r)

-- | Whether the readable side has been paused.
isPaused :: forall r e w. Readable w -> RIO r e Boolean
isPaused r = liftEffect (NS.isPaused r)

-- | Pipe a readable into a writable, ending the writable when the
-- | readable ends.
pipe :: forall r e w x. Readable w -> Writable x -> RIO r e Unit
pipe r w = liftEffect (NS.pipe r w)

-- | `pipe` with an explicit `end` flag controlling whether the
-- | writable should be ended when the readable finishes.
pipe'
  :: forall r e w x
   . Readable w
  -> Writable x
  -> { end :: Boolean }
  -> RIO r e Unit
pipe' r w opts = liftEffect (NS.pipe' r w opts)

-- | Detach a single previously-piped writable.
unpipe :: forall r e w x. Readable w -> Writable x -> RIO r e Unit
unpipe r w = liftEffect (NS.unpipe r w)

-- | Detach every previously-piped writable.
unpipeAll :: forall r e w. Readable w -> RIO r e Unit
unpipeAll r = liftEffect (NS.unpipeAll r)

-- | Try to pull a chunk from the readable buffer. Returns
-- | `Nothing` if no data is currently available.
read :: forall r e w. Readable w -> RIO r e (Maybe Buffer)
read r = liftEffect (NS.read r)

-- | `read` with an explicit size hint.
read' :: forall r e w. Readable w -> Int -> RIO r e (Maybe Buffer)
read' r size = liftEffect (NS.read' r size)

-- | `read`, decoding the resulting buffer as a `String`.
readString
  :: forall r e w
   . Readable w
  -> Encoding
  -> RIO r e (Maybe String)
readString r enc = liftEffect (NS.readString r enc)

-- | `read'`, decoding the resulting buffer as a `String`.
readString'
  :: forall r e w
   . Readable w
  -> Int
  -> Encoding
  -> RIO r e (Maybe String)
readString' r size enc = liftEffect (NS.readString' r size enc)

-- | `read` that tolerates a `setEncoding`-decorated stream.
readEither
  :: forall r e w
   . Readable w
  -> RIO r e (Maybe (Either String Buffer))
readEither r = liftEffect (NS.readEither r)

-- | `read'` that tolerates a `setEncoding`-decorated stream.
readEither'
  :: forall r e w
   . Readable w
  -> Int
  -> RIO r e (Maybe (Either String Buffer))
readEither' r size = liftEffect (NS.readEither' r size)

-- | Set the encoding chunks should be decoded with.
setEncoding
  :: forall r e w
   . Readable w
  -> Encoding
  -> RIO r e Unit
setEncoding r enc = liftEffect (NS.setEncoding r enc)

-- | Whether the writable side is open for writes.
writeable :: forall r e x. Writable x -> RIO r e Boolean
writeable w = liftEffect (NS.writeable w)

-- | Whether `end` has been called on the writable side.
writeableEnded :: forall r e x. Writable x -> RIO r e Boolean
writeableEnded w = liftEffect (NS.writeableEnded w)

-- | Whether the writable side is currently corked.
writeableCorked :: forall r e x. Writable x -> RIO r e Boolean
writeableCorked w = liftEffect (NS.writeableCorked w)

-- | Whether the stream has emitted `error`.
errored :: forall r e rw. Stream rw -> RIO r e Boolean
errored s = liftEffect (NS.errored s)

-- | Whether the writable side has fully drained.
writeableFinished :: forall r e x. Writable x -> RIO r e Boolean
writeableFinished w = liftEffect (NS.writeableFinished w)

-- | The high-water mark configured for the writable side.
writeableHighWaterMark :: forall r e x. Writable x -> RIO r e Number
writeableHighWaterMark w = liftEffect (NS.writeableHighWaterMark w)

-- | The number of bytes currently queued for the writable side.
writeableLength :: forall r e x. Writable x -> RIO r e Number
writeableLength w = liftEffect (NS.writeableLength w)

-- | Whether the writable side needs a `drain` before accepting
-- | more data.
writeableNeedDrain :: forall r e x. Writable x -> RIO r e Boolean
writeableNeedDrain w = liftEffect (NS.writeableNeedDrain w)

-- | Write a buffer. Returns `true` if the chunk was flushed
-- | immediately and `false` if the caller should wait for `drainH`.
write :: forall r e x. Writable x -> Buffer -> RIO r e Boolean
write w b = liftEffect (NS.write w b)

-- | `write` with a flush-completed continuation.
write'
  :: forall r e x
   . Writable x
  -> Buffer
  -> (Maybe Error -> Effect Unit)
  -> RIO r e Boolean
write' w b cb = liftEffect (NS.write' w b cb)

-- | Write a string in the given encoding.
writeString
  :: forall r e x
   . Writable x
  -> Encoding
  -> String
  -> RIO r e Boolean
writeString w enc s = liftEffect (NS.writeString w enc s)

-- | `writeString` with a flush-completed continuation.
writeString'
  :: forall r e x
   . Writable x
  -> Encoding
  -> String
  -> (Maybe Error -> Effect Unit)
  -> RIO r e Boolean
writeString' w enc s cb = liftEffect (NS.writeString' w enc s cb)

-- | Buffer subsequent writes until `uncork` is called.
cork :: forall r e x. Writable x -> RIO r e Unit
cork w = liftEffect (NS.cork w)

-- | Flush data buffered by `cork`.
uncork :: forall r e x. Writable x -> RIO r e Unit
uncork w = liftEffect (NS.uncork w)

-- | Configure the default encoding used by `writeString` when no
-- | encoding is provided.
setDefaultEncoding
  :: forall r e x
   . Writable x
  -> Encoding
  -> RIO r e Unit
setDefaultEncoding w enc = liftEffect (NS.setDefaultEncoding w enc)

-- | Signal end-of-stream on the writable side.
end :: forall r e x. Writable x -> RIO r e Unit
end w = liftEffect (NS.end w)

-- | `end` with a finish-completed continuation.
end'
  :: forall r e x
   . Writable x
  -> (Maybe Error -> Effect Unit)
  -> RIO r e Unit
end' w cb = liftEffect (NS.end' w cb)

-- | Destroy the stream, releasing any held resources.
destroy :: forall r e rw. Stream rw -> RIO r e Unit
destroy s = liftEffect (NS.destroy s)

-- | `destroy` with an explicit error.
destroy' :: forall r e rw. Stream rw -> Error -> RIO r e Unit
destroy' s err = liftEffect (NS.destroy' s err)

-- | Whether the stream has been closed.
closed :: forall r e rw. Stream rw -> RIO r e Boolean
closed s = liftEffect (NS.closed s)

-- | Whether `destroy` has been called on the stream.
destroyed :: forall r e rw. Stream rw -> RIO r e Boolean
destroyed s = liftEffect (NS.destroyed s)

-- | Whether a duplex stream keeps the writable side open after the
-- | readable side has ended.
allowHalfOpen :: forall r e. Duplex -> RIO r e Boolean
allowHalfOpen d = liftEffect (NS.allowHalfOpen d)

-- | Pipe a readable through zero or more duplex transforms into a
-- | writable, calling the callback once the pipeline completes or
-- | fails.
pipeline
  :: forall r e w x
   . Readable w
  -> Array Duplex
  -> Writable x
  -> (Maybe Error -> Effect Unit)
  -> RIO r e Unit
pipeline src transforms dest cb =
  liftEffect (NS.pipeline src transforms dest cb)

-- | Build a `Readable` that emits the given string in the given
-- | encoding once and then ends.
readableFromString
  :: forall r e
   . String
  -> Encoding
  -> RIO r e (Readable ())
readableFromString s enc = liftEffect (NS.readableFromString s enc)

-- | Build a `Readable` that emits the given buffer once and then
-- | ends.
readableFromBuffer
  :: forall r e
   . Buffer
  -> RIO r e (Readable ())
readableFromBuffer b = liftEffect (NS.readableFromBuffer b)

-- | A fresh in-memory pass-through duplex. Useful for tests and for
-- | wiring a producer to a consumer without going through the OS.
newPassThrough :: forall r e. RIO r e Duplex
newPassThrough = liftEffect NS.newPassThrough

-- | Read everything from the stream as a UTF-8 `String`. Blocks the
-- | underlying `Aff` until `endH` or `closeH` fires.
readableToStringUtf8 :: forall r e w. Readable w -> RIO r e String
readableToStringUtf8 r = liftAff (NSAff.readableToStringUtf8 r)

-- | Read everything from the stream as a `String` in the given
-- | encoding.
readableToString
  :: forall r e w
   . Readable w
  -> Encoding
  -> RIO r e String
readableToString r enc = liftAff (NSAff.readableToString r enc)

-- | Read everything from the stream as an array of buffers.
readableToBuffers
  :: forall r e w
   . Readable w
  -> RIO r e (Array Buffer)
readableToBuffers r = liftAff (NSAff.readableToBuffers r)

-- | Wait for some data to be available on the stream and read what
-- | is buffered. Returns whatever was buffered plus a flag
-- | indicating whether the stream may still have more.
readSome
  :: forall r e w
   . Readable w
  -> RIO r e { buffers :: Array Buffer, readagain :: Boolean }
readSome r = liftAff (NSAff.readSome r)

-- | Read every chunk the stream produces until it ends.
readAll
  :: forall r e w
   . Readable w
  -> RIO r e (Array Buffer)
readAll r = liftAff (NSAff.readAll r)

-- | Try to read exactly `n` bytes from the stream. If the stream
-- | ends first the result has fewer than `n` bytes.
readN
  :: forall r e w
   . Readable w
  -> Int
  -> RIO r e { buffers :: Array Buffer, readagain :: Boolean }
readN r n = liftAff (NSAff.readN r n)

-- | Write an array of buffers to a writable, awaiting `drainH`
-- | between chunks so that the caller never overflows the
-- | high-water mark. Use this when streaming arbitrary amounts of
-- | data into a slow sink.
writeAll
  :: forall r e x
   . Writable x
  -> Array Buffer
  -> RIO r e Unit
writeAll w bs = liftAff (NSAff.write w bs)

-- | Signal end-of-stream and wait for `finishH` (i.e. for every
-- | queued chunk to be flushed and any `autoClose`-backed teardown
-- | to run).
endAwait :: forall r e x. Writable x -> RIO r e Unit
endAwait w = liftAff (NSAff.end w)

-- | Concatenate an array of UTF-8 encoded buffers into a string.
toStringUTF8 :: forall r e. Array Buffer -> RIO r e String
toStringUTF8 bs = liftEffect (NSAff.toStringUTF8 bs)

-- | Convert a UTF-8 string into the corresponding one-element
-- | buffer array.
fromStringUTF8 :: forall r e. String -> RIO r e (Array Buffer)
fromStringUTF8 s = liftEffect (NSAff.fromStringUTF8 s)
