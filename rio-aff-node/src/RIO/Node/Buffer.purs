-- | RIO-flavoured wrappers around `Node.Buffer`.
-- |
-- | A `Buffer` is a mutable value, not a capability. So rather than
-- | hiding allocation behind a service row, we simply lift each
-- | `Effect`-valued operation into `RIO`. `slice` (the only genuinely
-- | pure binding) stays pure here.
-- |
-- | The `Encoding`, `BufferValueType`, `Octet`, and `Offset` types
-- | from `node-buffer` are re-exported as-is so callers do not need
-- | to import `Node.Buffer.*` modules separately.
module RIO.Aff.Node.Buffer
  ( module Exports
  , alloc
  , allocUnsafe
  , allocUnsafeSlow
  , compareParts
  , concat
  , concat'
  , copy
  , create
  , fill
  , freeze
  , fromArray
  , fromArrayBuffer
  , fromString
  , getAtOffset
  , poolSize
  , read
  , readString
  , setAtOffset
  , setPoolSize
  , size
  , slice
  , swap16
  , swap32
  , swap64
  , thaw
  , toArray
  , toArrayBuffer
  , toString
  , toString'
  , transcode
  , unsafeFreeze
  , unsafeThaw
  , write
  , writeString
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer) as Exports
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Node.Buffer.Immutable (ImmutableBuffer) as Exports
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Types (BufferValueType(..), Octet, Offset) as Exports
import Node.Buffer.Types (BufferValueType, Octet, Offset)
import Node.Encoding (Encoding) as Exports
import Node.Encoding (Encoding)

import RIO.Aff.Core (RIO)

-- | Allocate a new buffer of the specified size. Alias for `alloc`.
create :: forall r e. Int -> RIO r e Buffer
create n = liftEffect (NB.create n)

-- | Allocate a new zero-filled buffer of the specified size.
alloc :: forall r e. Int -> RIO r e Buffer
alloc n = liftEffect (NB.alloc n)

-- | Allocate from Node's internal pool; the buffer is uninitialized
-- | and may contain previously-allocated data. See the Node docs for
-- | safety implications.
allocUnsafe :: forall r e. Int -> RIO r e Buffer
allocUnsafe n = liftEffect (NB.allocUnsafe n)

-- | Allocate outside Node's internal pool; the buffer is uninitialized.
allocUnsafeSlow :: forall r e. Int -> RIO r e Buffer
allocUnsafeSlow n = liftEffect (NB.allocUnsafeSlow n)

-- | Compare slices of two buffers and return their relative ordering.
compareParts
  :: forall r e
   . Buffer
  -> Buffer
  -> Offset
  -> Offset
  -> Offset
  -> Offset
  -> RIO r e Ordering
compareParts src tgt tStart tEnd sStart sEnd =
  liftEffect (NB.compareParts src tgt tStart tEnd sStart sEnd)

-- | Produce an immutable copy of a buffer.
freeze :: forall r e. Buffer -> RIO r e ImmutableBuffer
freeze b = liftEffect (NB.freeze b)

-- | O(1) cast to `ImmutableBuffer` without copying. The mutable
-- | buffer must not be used afterwards.
unsafeFreeze :: forall r e. Buffer -> RIO r e ImmutableBuffer
unsafeFreeze b = liftEffect (NB.unsafeFreeze b)

-- | Produce a mutable copy of an immutable buffer.
thaw :: forall r e. ImmutableBuffer -> RIO r e Buffer
thaw b = liftEffect (NB.thaw b)

-- | O(1) cast to `Buffer` without copying. The immutable buffer
-- | must not be used afterwards.
unsafeThaw :: forall r e. ImmutableBuffer -> RIO r e Buffer
unsafeThaw b = liftEffect (NB.unsafeThaw b)

-- | Build a new buffer from an array of octets.
fromArray :: forall r e. Array Octet -> RIO r e Buffer
fromArray xs = liftEffect (NB.fromArray xs)

-- | Build a new buffer by encoding the given string.
fromString :: forall r e. String -> Encoding -> RIO r e Buffer
fromString s enc = liftEffect (NB.fromString s enc)

-- | Wrap a JS `ArrayBuffer` as a `Buffer` without copying.
fromArrayBuffer :: forall r e. ArrayBuffer -> RIO r e Buffer
fromArrayBuffer ab = liftEffect (NB.fromArrayBuffer ab)

-- | Copy a buffer's contents into a fresh `ArrayBuffer`.
toArrayBuffer :: forall r e. Buffer -> RIO r e ArrayBuffer
toArrayBuffer b = liftEffect (NB.toArrayBuffer b)

-- | Read a numeric value of the given type at the specified offset.
read :: forall r e. BufferValueType -> Offset -> Buffer -> RIO r e Number
read ty o b = liftEffect (NB.read ty o b)

-- | Decode a range of the buffer as a string.
readString
  :: forall r e
   . Encoding
  -> Offset
  -> Offset
  -> Buffer
  -> RIO r e String
readString enc s e b = liftEffect (NB.readString enc s e b)

-- | Decode the entire buffer as a string.
toString :: forall r e. Encoding -> Buffer -> RIO r e String
toString enc b = liftEffect (NB.toString enc b)

-- | Decode a slice of the buffer as a string.
toString'
  :: forall r e
   . Encoding
  -> Offset
  -> Offset
  -> Buffer
  -> RIO r e String
toString' enc s e b = liftEffect (NB.toString' enc s e b)

-- | Write a numeric value to a buffer at the specified offset.
write
  :: forall r e
   . BufferValueType
  -> Number
  -> Offset
  -> Buffer
  -> RIO r e Unit
write ty v o b = liftEffect (NB.write ty v o b)

-- | Encode a string into the buffer at the specified offset and
-- | return the number of bytes written.
writeString
  :: forall r e
   . Encoding
  -> Offset
  -> Int
  -> String
  -> Buffer
  -> RIO r e Int
writeString enc o n s b = liftEffect (NB.writeString enc o n s b)

-- | Copy the buffer's contents into an `Array Octet`.
toArray :: forall r e. Buffer -> RIO r e (Array Octet)
toArray b = liftEffect (NB.toArray b)

-- | Look up the octet at the given offset, if any.
getAtOffset :: forall r e. Offset -> Buffer -> RIO r e (Maybe Octet)
getAtOffset o b = liftEffect (NB.getAtOffset o b)

-- | Write an octet at the given offset.
setAtOffset :: forall r e. Octet -> Offset -> Buffer -> RIO r e Unit
setAtOffset v o b = liftEffect (NB.setAtOffset v o b)

-- | Create a sliding view over a buffer. Pure, because no allocation
-- | takes place: writes to the slice mutate the original buffer.
slice :: Offset -> Offset -> Buffer -> Buffer
slice = NB.slice

-- | Length of the buffer in bytes.
size :: forall r e. Buffer -> RIO r e Int
size b = liftEffect (NB.size b)

-- | Concatenate an array of buffers into a single new buffer.
concat :: forall r e. Array Buffer -> RIO r e Buffer
concat xs = liftEffect (NB.concat xs)

-- | Concatenate an array of buffers, producing a result of exactly
-- | the requested length.
concat' :: forall r e. Array Buffer -> Int -> RIO r e Buffer
concat' xs n = liftEffect (NB.concat' xs n)

-- | Copy a region of `src` into `target` and return the number of
-- | bytes copied.
copy
  :: forall r e
   . Offset
  -> Offset
  -> Buffer
  -> Offset
  -> Buffer
  -> RIO r e Int
copy sStart sEnd src tStart tgt =
  liftEffect (NB.copy sStart sEnd src tStart tgt)

-- | Fill a region of the buffer with the given octet.
fill :: forall r e. Octet -> Offset -> Offset -> Buffer -> RIO r e Unit
fill v s e b = liftEffect (NB.fill v s e b)

-- | Current size of Node's internal buffer pool, in bytes.
poolSize :: forall r e. RIO r e Int
poolSize = liftEffect NB.poolSize

-- | Set the size of Node's internal buffer pool, in bytes.
setPoolSize :: forall r e. Int -> RIO r e Unit
setPoolSize n = liftEffect (NB.setPoolSize n)

-- | Byte-swap every 16-bit segment of the buffer in place; returns
-- | the same buffer for chaining.
swap16 :: forall r e. Buffer -> RIO r e Buffer
swap16 b = liftEffect (NB.swap16 b)

-- | Byte-swap every 32-bit segment of the buffer in place.
swap32 :: forall r e. Buffer -> RIO r e Buffer
swap32 b = liftEffect (NB.swap32 b)

-- | Byte-swap every 64-bit segment of the buffer in place.
swap64 :: forall r e. Buffer -> RIO r e Buffer
swap64 b = liftEffect (NB.swap64 b)

-- | Transcode the buffer from one encoding to another, producing a
-- | new buffer.
transcode :: forall r e. Buffer -> Encoding -> Encoding -> RIO r e Buffer
transcode b from to = liftEffect (NB.transcode b from to)
