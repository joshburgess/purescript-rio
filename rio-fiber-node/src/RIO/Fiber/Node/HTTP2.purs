-- | RIO-flavoured wrappers around `Node.Http2`.
-- |
-- | An `Http2SecureServer`, `Http2Session endpoint`, and
-- | `Http2Stream endpoint` are each values (handles on the
-- | underlying TLS / session / stream resources) rather than
-- | capabilities, so the upstream surface is mirrored across
-- | several sub-modules by lifting each `Effect`-valued primitive
-- | into `RIO`. This top-level module re-exports the most
-- | commonly used types and pure values from the underlying
-- | sub-modules so most call sites can just `import RIO.Fiber.Node.HTTP2`.
module RIO.Fiber.Node.HTTP2
  ( module Exports
  ) where

import Node.Http2.ErrorCode
  ( ErrorCode(..)
  , cancel
  , compressionError
  , connectError
  , enhanceYourCalm
  , flowControlError
  , frameSizeError
  , http1_1Required
  , inadequateSecurity
  , internalError
  , noError
  , protocolError
  , refusedStream
  , settingsTimeout
  , streamClosed
  ) as Exports
import Node.Http2.Flags
  ( BitwiseFlag
  , ack
  , continuationFlags
  , dataFlags
  , enable
  , endHeaders
  , endStream
  , headersFlags
  , isDisabled
  , isEnabled
  , padded
  , pingFlags
  , priority
  , printFlags
  , printFlags'
  , pushPromiseFlags
  , settingsFlags
  , unFlag
  ) as Exports
import Node.Http2.FrameType
  ( FrameType(..)
  , frameContinuation
  , frameData
  , frameGoAway
  , frameHeaders
  , framePing
  , framePriority
  , framePushPromise
  , frameRstStream
  , frameSettings
  , frameWindowUpdate
  ) as Exports
import Node.Http2.PaddingStrategy
  ( PaddingStrategy(..)
  ) as Exports
import Node.Http2.Types
  ( Headers
  , Http2ClientConnectOptions
  , Http2CreateSecureServerOptions
  , Http2SecureServer
  , Http2ServerRequest
  , Http2ServerResponse
  , Http2Session
  , Http2Stream
  , Settings
  , StreamId(..)
  , connectionId
  ) as Exports
