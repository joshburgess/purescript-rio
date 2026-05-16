-- | W3C Trace Context propagation: parse and format the
-- | `traceparent` / `tracestate` HTTP headers, and generate fresh
-- | trace and span IDs in the canonical 16- and 8-byte hex shapes.
-- |
-- | This module is a small, dependency-free utility. It does not
-- | mutate the `RIO.Tracer` service; instead, an application
-- | parses an incoming `traceparent` at request boundaries, uses
-- | the contained `traceId` / `parentId` to seed a fresh span,
-- | and formats an outgoing `traceparent` whenever it crosses
-- | another process boundary.
-- |
-- | The format is:
-- |
-- | ```
-- | <version>-<trace-id>-<parent-id>-<flags>
-- | ```
-- |
-- | with `version = "00"`, `trace-id` 32 lowercase hex characters,
-- | `parent-id` 16 lowercase hex characters, and `flags` two hex
-- | characters whose low bit is the "sampled" flag. Trace IDs of
-- | all zeros and span IDs of all zeros are invalid (the spec
-- | reserves them as "unset"); the parser rejects both.
module RIO.Tracer.Propagation
  ( TraceContext
  , parseTraceparent
  , formatTraceparent
  , parseTracestate
  , formatTracestate
  , newTraceId
  , newSpanId
  , withSampled
  , isSampled
  ) where

import Prelude

import Data.Array as Array
import Data.Int (Radix, fromStringAs, toStringAs)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)

-- | A parsed W3C trace context. `traceId` is 32 lowercase hex
-- | characters, `spanId` is 16 lowercase hex characters,
-- | `sampled` reflects the low bit of `trace-flags`. `tracestate`
-- | is a list of vendor-specific key/value pairs in declaration
-- | order (the header carries at most 32 entries; oversized
-- | inputs are truncated by `parseTracestate`).
type TraceContext =
  { traceId :: String
  , spanId :: String
  , sampled :: Boolean
  , tracestate :: Array (Tuple String String)
  }

hexRadix :: Radix
hexRadix = case Int.radix 16 of
  Just r -> r
  Nothing -> Int.decimal

-- | Parse a `traceparent` header value. Returns `Nothing` if the
-- | header does not match the W3C format, or if either ID is the
-- | reserved all-zero value.
-- |
-- | The supplied `tracestate` field is left empty; pair with
-- | `parseTracestate` if both headers were provided.
parseTraceparent :: String -> Maybe TraceContext
parseTraceparent input = case String.split (Pattern "-") (String.trim input) of
  [ version, traceId, spanId, flags ]
    | version == "00"
    , isHexOfLength 32 traceId
    , isHexOfLength 16 spanId
    , isHexOfLength 2 flags
    , not (allZero traceId)
    , not (allZero spanId) ->
        case fromStringAs hexRadix flags of
          Nothing -> Nothing
          Just f -> Just
            { traceId
            , spanId
            , sampled: (f `mod` 2) /= 0
            , tracestate: []
            }
  _ -> Nothing

-- | Format a `TraceContext` as a `traceparent` header value.
-- | Always emits version `"00"` and reflects `sampled` into the
-- | low bit of `trace-flags`.
formatTraceparent :: TraceContext -> String
formatTraceparent ctx =
  "00-" <> ctx.traceId <> "-" <> ctx.spanId <> "-" <> flagsHex
  where
  flagsHex =
    let
      bits = if ctx.sampled then 1 else 0
      raw = toStringAs hexRadix bits
    in
      if CU.length raw < 2 then padLeft 2 '0' raw else raw

-- | Parse a `tracestate` header value as a list of `key=value`
-- | pairs in declaration order. Whitespace around keys and values
-- | is stripped; entries without an `=` are dropped. The W3C
-- | maximum entry count is 32; longer lists are truncated.
parseTracestate :: String -> Array (Tuple String String)
parseTracestate input =
  Array.take 32
    $ Array.mapMaybe parseEntry
    $ map String.trim
    $ String.split (Pattern ",")
    $ String.trim input
  where
  parseEntry s = case String.indexOf (Pattern "=") s of
    Nothing -> Nothing
    Just ix ->
      let
        k = String.trim (CU.take ix s)
        v = String.trim (CU.drop (ix + 1) s)
      in
        if k == "" then Nothing else Just (Tuple k v)

-- | Format a list of `tracestate` entries as the corresponding
-- | header value (comma-separated `key=value` pairs).
formatTracestate :: Array (Tuple String String) -> String
formatTracestate =
  String.joinWith ","
    <<< map (\(Tuple k v) -> k <> "=" <> v)

-- | Generate a fresh 16-byte trace ID, emitted as 32 lowercase
-- | hex characters. Uses `Effect.Random.randomInt`, which is
-- | seeded by `Math.random` and is *not* cryptographically
-- | secure; production OTel exporters should swap this for a
-- | crypto-backed generator wired through a dedicated service.
newTraceId :: Effect String
newTraceId = randomHex 16

-- | Generate a fresh 8-byte span ID, emitted as 16 lowercase hex
-- | characters. Same caveat as `newTraceId`: not crypto-secure.
newSpanId :: Effect String
newSpanId = randomHex 8

-- | Set the `sampled` flag on a `TraceContext`.
withSampled :: Boolean -> TraceContext -> TraceContext
withSampled b ctx = ctx { sampled = b }

-- | Read the `sampled` flag from a `TraceContext`.
isSampled :: TraceContext -> Boolean
isSampled = _.sampled

randomHex :: Int -> Effect String
randomHex byteCount = go byteCount ""
  where
  go n acc
    | n <= 0 = pure acc
    | otherwise = do
        b <- randomInt 0 255
        let
          raw = toStringAs hexRadix b
          padded = if CU.length raw < 2 then padLeft 2 '0' raw else raw
        go (n - 1) (acc <> padded)

isHexOfLength :: Int -> String -> Boolean
isHexOfLength n s = CU.length s == n && allHex s

allHex :: String -> Boolean
allHex s = Array.all isHexChar (CU.toCharArray s)

isHexChar :: Char -> Boolean
isHexChar c =
  (c >= '0' && c <= '9')
    || (c >= 'a' && c <= 'f')
    || (c >= 'A' && c <= 'F')

allZero :: String -> Boolean
allZero s = CU.length (String.replaceAll (Pattern "0") (String.Replacement "") s) == 0

padLeft :: Int -> Char -> String -> String
padLeft target c s =
  let
    deficit = target - CU.length s
  in
    if deficit <= 0 then s
    else CU.fromCharArray (Array.replicate deficit c) <> s

