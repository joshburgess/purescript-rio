-- Case: `Channel.pipe` composes upstream and downstream end-to-end.
-- The upstream's output element type must unify with the
-- downstream's input element type; otherwise the pipe is not
-- type-correct.
--
-- Here the upstream is `fromStream (Stream.fromArray [1, 2, 3])`
-- (emits `Int`) and the downstream is `fromSink Sink.collect`
-- specialised at `String` (reads `String`). The intermediate
-- element type cannot unify Int with String. The compiler must
-- reject the pipe.
module Scratch where

import Prelude

import Effect.Aff (Aff)

import RIO.Channel (Channel, fromSink, fromStream, pipe, run)
import RIO.Core (runRIO')
import RIO.Sink (Sink, collect)
import RIO.Stream as Stream

upstream :: forall r e i. Channel r e i Int Unit
upstream = fromStream (Stream.fromArray [ 1, 2, 3 ])

downstream :: forall r e o. Channel r e String o (Array String)
downstream = fromSink (collect :: forall r' e'. Sink r' e' String (Array String))

-- Upstream emits Int; downstream reads String. The intermediate
-- type cannot unify, so this pipe must be rejected.
result :: Aff (Array String)
result = runRIO' (run (pipe upstream downstream))
