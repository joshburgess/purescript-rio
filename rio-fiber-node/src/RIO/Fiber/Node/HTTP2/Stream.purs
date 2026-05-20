-- | RIO-flavoured wrappers around `Node.Http2.Stream`.
-- |
-- | An `Http2Stream endpoint` is a value (a handle on a single
-- | HTTP/2 stream), not a capability, so every `Effect` operation
-- | is lifted directly into `RIO`. Event handles and the
-- | `toDuplex` projection are re-exported unchanged.
module RIO.Fiber.Node.HTTP2.Stream
  ( module Exports
  , Http2StreamState
  , PriorityOptions
  , PushStreamOptions
  , RespondOptions
  , RespondWithFdOptions
  , RespondWithFileOptions
  , additionalHeaders
  , bufferSize
  , close
  , closed
  , destroyed
  , endAfterHeaders
  , headersSent
  , id
  , pending
  , priority
  , pushAllowed
  , pushStream
  , pushStream'
  , respond
  , respondWithFd
  , respondWithFile
  , rstCode
  , sendTrailers
  , sentHeaders
  , sentInfoHeaders
  , sentTrailers
  , session
  , setTimeout
  , state
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Node.FS (FileDescriptor)
import Node.Http2.ErrorCode (ErrorCode)
import Node.Http2.Stream
  ( Http2StreamState
  , PriorityOptions
  , PushStreamOptions
  , RespondOptions
  , RespondWithFdOptions
  , RespondWithFileOptions
  ) as RawStrm
import Node.Http2.Stream
  ( abortedH
  , closeH
  , continueH
  , errorH
  , frameErrorH
  , headersH
  , pushH
  , readyH
  , responseH
  , timeoutH
  , toDuplex
  , trailersH
  , wantTrailersH
  ) as Exports
import Node.Http2.Stream as Strm
import Node.Http2.Types
  ( Headers
  , Http2Session
  , Http2Stream
  , StreamId
  )
import Node.Path (FilePath)
import Node.TLS.Types (Server)

import RIO.Fiber.Core (RIO, liftEffect)

type Http2StreamState = RawStrm.Http2StreamState

type PriorityOptions = RawStrm.PriorityOptions

type PushStreamOptions = RawStrm.PushStreamOptions

type RespondOptions = RawStrm.RespondOptions

type RespondWithFdOptions = RawStrm.RespondWithFdOptions

type RespondWithFileOptions = RawStrm.RespondWithFileOptions

-- | The current number of bytes buffered on the stream.
bufferSize :: forall r e ep. Http2Stream ep -> RIO r e Int
bufferSize s = liftEffect (Strm.bufferSize s)

-- | Close the stream with the given error code.
close
  :: forall r e ep. Http2Stream ep -> ErrorCode -> RIO r e Unit
close s c = liftEffect (Strm.close s c)

-- | Whether the stream is closed.
closed :: forall r e ep. Http2Stream ep -> RIO r e Boolean
closed s = liftEffect (Strm.closed s)

-- | Whether the stream has been destroyed.
destroyed :: forall r e ep. Http2Stream ep -> RIO r e Boolean
destroyed s = liftEffect (Strm.destroyed s)

-- | Whether the stream closes after the headers are received.
endAfterHeaders
  :: forall r e ep. Http2Stream ep -> RIO r e Boolean
endAfterHeaders s = liftEffect (Strm.endAfterHeaders s)

-- | The numeric stream id, if one has been assigned.
id
  :: forall r e ep. Http2Stream ep -> RIO r e (Maybe StreamId)
id s = liftEffect (Strm.id s)

-- | Whether the stream is still pending.
pending :: forall r e ep. Http2Stream ep -> RIO r e Boolean
pending s = liftEffect (Strm.pending s)

-- | Send a PRIORITY frame describing this stream's relative
-- | priority.
priority
  :: forall r e ep
   . Http2Stream ep
  -> PriorityOptions
  -> RIO r e Unit
priority s p = liftEffect (Strm.priority s p)

-- | The reset code the peer used to close this stream, if any.
rstCode
  :: forall r e ep
   . Http2Stream ep
  -> RIO r e (Maybe ErrorCode)
rstCode s = liftEffect (Strm.rstCode s)

-- | The headers the stream sent.
sentHeaders :: forall r e ep. Http2Stream ep -> RIO r e Headers
sentHeaders s = liftEffect (Strm.sentHeaders s)

-- | The informational headers the stream sent.
sentInfoHeaders
  :: forall r e ep. Http2Stream ep -> RIO r e (Array Headers)
sentInfoHeaders s = liftEffect (Strm.sentInfoHeaders s)

-- | The trailers the stream sent.
sentTrailers :: forall r e ep. Http2Stream ep -> RIO r e Headers
sentTrailers s = liftEffect (Strm.sentTrailers s)

-- | The session this stream belongs to.
session
  :: forall r e ep
   . Http2Stream ep
  -> RIO r e (Maybe (Http2Session ep))
session s = liftEffect (Strm.session s)

-- | Arm the inactivity timer.
setTimeout
  :: forall r e ep
   . Http2Stream ep
  -> Milliseconds
  -> Effect Unit
  -> RIO r e Unit
setTimeout s ms cb = liftEffect (Strm.setTimeout s ms cb)

-- | Detailed stream-state numbers.
state :: forall r e ep. Http2Stream ep -> RIO r e Http2StreamState
state s = liftEffect (Strm.state s)

-- | Send trailers on this stream.
sendTrailers
  :: forall r e ep. Http2Stream ep -> Headers -> RIO r e Unit
sendTrailers s t = liftEffect (Strm.sendTrailers s t)

-- | Server-side: send an additional headers frame.
additionalHeaders
  :: forall r e
   . Http2Stream Server
  -> Headers
  -> RIO r e Unit
additionalHeaders s h = liftEffect (Strm.additionalHeaders s h)

-- | Server-side: whether the response headers have been sent.
headersSent :: forall r e. Http2Stream Server -> RIO r e Boolean
headersSent s = liftEffect (Strm.headersSent s)

-- | Server-side: whether pushing new streams is allowed.
pushAllowed :: forall r e. Http2Stream Server -> RIO r e Boolean
pushAllowed s = liftEffect (Strm.pushAllowed s)

-- | Server-side: push a new server-initiated stream.
pushStream
  :: forall r e
   . Http2Stream Server
  -> Headers
  -> ( Either Error (Http2Stream Server)
       -> Headers
       -> Effect Unit
     )
  -> RIO r e Unit
pushStream s h cb = liftEffect (Strm.pushStream s h cb)

-- | Server-side: push a new server-initiated stream with
-- | explicit `PushStreamOptions`.
pushStream'
  :: forall r e
   . Http2Stream Server
  -> Headers
  -> PushStreamOptions
  -> ( Either Error (Http2Stream Server)
       -> Headers
       -> Effect Unit
     )
  -> RIO r e Unit
pushStream' s h opts cb = liftEffect (Strm.pushStream' s h opts cb)

-- | Server-side: respond on this stream.
respond
  :: forall r e
   . Http2Stream Server
  -> Headers
  -> RespondOptions
  -> RIO r e Unit
respond s h o = liftEffect (Strm.respond s h o)

-- | Server-side: respond on this stream with the contents of a
-- | file descriptor.
respondWithFd
  :: forall r e
   . Http2Stream Server
  -> FileDescriptor
  -> Headers
  -> RespondWithFdOptions
  -> RIO r e Unit
respondWithFd s fd h o = liftEffect (Strm.respondWithFd s fd h o)

-- | Server-side: respond on this stream with the contents of a
-- | file path.
respondWithFile
  :: forall r e
   . Http2Stream Server
  -> FilePath
  -> Headers
  -> RespondWithFileOptions
  -> RIO r e Unit
respondWithFile s fp h o = liftEffect (Strm.respondWithFile s fp h o)
