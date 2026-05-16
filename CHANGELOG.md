# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
once it reaches `1.0.0`. While in the `0.x` series, minor releases may include
breaking changes (see `CONTRIBUTING.md`, "Versioning Policy").

## [Unreleased]

### Added

- `RIO.Query`: a request-batching loader in the DataLoader
  family. Each fiber calls `load loader key` and the loader
  queues the key in a pending set; on the next macrotask
  (`Aff.delay 0`) it flushes the set through a user-supplied
  `batchFn :: Array k -> Aff (Map k v)` and resolves every
  awaiter with the right value. Concurrent loads of the same
  key dedupe to a single `Deferred`; an optional in-loader
  cache serves repeats without re-fetching. `loadMany` fetches
  many keys through the same loader in parallel; `loadOpt`
  returns `Nothing` for missing keys instead of raising;
  `prime` / `clear` / `clearAll` / `size` manage the cache.
  `maxBatchSize` caps each `batchFn` call. Missing keys surface
  as `QueryMissingKey`; a rejected `batchFn` surfaces as
  `QueryBatchFailure` on every awaiter in the batch.
- `RIO.HttpClient`: a pluggable HTTP client service. Defines the
  shape (`HttpClient` service record, `Request` / `Response`
  records, `Method` sum, `RequestBody` sum, `HttpError` sum with
  transport / timeout / unexpected-status / Schema-decode arms)
  so application code can be written against a single API
  regardless of the wired-in backend. Ships `newRequest`,
  `withMethod`, `withHeader` / `withHeaders`, `withBody`,
  `withJsonBody` (adds `Content-Type: application/json` if the
  request does not already carry one), `withTimeout`; smart
  constructors for each method (`get`, `post`, `put`, `patch`,
  `delete`, `head_`, `options`); `decodeBody` for
  `Schema`-decoded responses; `statusClass` / `isSuccess` /
  `ensureStatus` for status handling; and `mockHttpClient` for
  tests. The error row tag is fixed to `httpError`, so callers
  pair the client with `RIO.Schedule.retry`, `RIO.Tracer.withSpan`,
  and `RIO.Logger.withFields` to layer in retries, tracing, and
  structured logs without the client itself having to bake any
  of those in.
- `RIO.Schema`: a small runtime schema library built on top of
  `argonaut-core`. A `Schema a` carries both a decoder
  (`Json -> Either DecodeError a`) and an encoder (`a -> Json`),
  so the same value defines the wire format in both directions.
  Primitive schemas (`string`, `int`, `number`, `boolean`,
  `null_`) describe individual JSON kinds; combinators (`array`,
  `nullable`, `transform`, `refine`, `union`, `enum`) compose
  them. Records are described through an `Applicative` builder
  (`RecordSchema r a`): each `field` declares how to pull a value
  out of the target record and which schema validates it,
  `fieldOpt` / `fieldDefault` cover absence and defaults, and
  `recordOf` collapses the builder into a `Schema`. `parseJson`
  parses-and-decodes in one step (returning `ParseError` for
  invalid JSON); `DecodeError` accumulates a `$.field[0]`-style
  path that `renderError` turns into a short human-readable
  trail (`expected string at $.user[2], got number`).
- `RIO.RateLimiter`: a token-bucket rate limiter built on top of
  `RIO.Clock`. `make` allocates a bucket configured by
  `{ permitsPerSecond, burst }`; `acquire` / `acquireN` block
  until enough tokens have refilled and `sleep` for the
  computed wait through the Clock service; `tryAcquire` /
  `tryAcquireN` return immediately with a `Boolean` indicating
  whether the deduction succeeded; `withPermit` / `withPermits`
  bracket an action with an acquire (no release because tokens
  are spent, not borrowed); `available` exposes the
  approximate current token count. The limiter is fully driven
  by `RIO.Test.Clock` for deterministic tests; under contention
  ordering is approximate (no FIFO wait queue), and callers who
  need strict fairness should layer a `Semaphore` on top.
- `RIO.Time`: small ergonomics layer over `RIO.Clock` and
  `Data.Time.Duration`. Adds an `Instant` newtype around
  `Milliseconds` (epoch-anchored timestamps, distinguished from
  the `Milliseconds`-shaped `Duration` so a "moment in time" and
  a "length of time" can no longer be silently confused),
  `Duration` smart constructors (`milliseconds`, `seconds`,
  `minutes`, `hours`, `days`), instant arithmetic
  (`addDuration`, `subDuration`, `between`, `diffMs`),
  millisecond-precision ISO 8601 round-trip (`formatISO8601`,
  `parseISO8601`) that goes through the host `Date` constructor,
  and a `humanize` renderer for `Duration` values
  (`"1d 2h 3m 4s"`, `"500ms"`, `"-1s"`). `nowInstant` reads the
  current `Instant` off the existing `Clock` service so it works
  unchanged with `RIO.Test.Clock` for deterministic tests.
- `rio-node`: new sibling package wrapping the Node.js standard
  library bindings as RIO services. First three modules shipped:
  * `RIO.Node.FileSystem` — service exposing the full surface
    of `Node.FS.Aff` (read / write / append / `stat` / `lstat` /
    `readdir` / `mkdir` / `mkdir'` / `rm` / `rm'` / `rmdir` /
    `rmdir'` / `mkdtemp` / `mkdtemp'` / `rename` / `unlink` /
    `link` / `symlink` / `readlink` / `realpath` / `realpath'` /
    `chmod` / `chown` / `truncate` / `utimes` / `access` /
    `access'` / `copyFile` / `copyFile'` / `fdOpen` / `fdRead` /
    `fdWrite` / `fdNext` / `fdAppend` / `fdClose`) with a
    `liveFileSystem` implementation backed by `node-fs`.
  * `RIO.Node.Path` — thin convenience wrappers around
    `Node.Path`. Pure functions are re-exported verbatim;
    `resolve` is lifted into `RIO` because `Node.Path.resolve`
    reads `process.cwd()`.
  * `RIO.Node.OS` — service over the read-only and
    priority-control surface of `Node.OS` (`arch`, `cpus`,
    `endianness`, `freemem`, `getPriority` /
    `setPriority` / `getCurrentProcessPriority` /
    `setCurrentProcessPriority`, `homedir`, `hostname`,
    `loadavg`, `machine`, `networkInterfaces`, `platform`,
    `release`, `tmpdir`, `totalmem`, `uptime`, `userInfo`,
    `version`) with a `liveOS` implementation. `eol`,
    `devNull`, and `constants` are re-exported as pure values.
  * `RIO.Node.URL` — RIO-flavoured wrappers around `Node.URL`.
    `Node.URL.URL` is a mutable handle whose accessors are
    `Effect`-valued, so every operation is lifted into `RIO`;
    `canParse` and `origin` (the only genuinely pure ones)
    stay pure.
  * `RIO.Node.Process` — service over the practical subset of
    `Node.Process` (argv / cwd / chdir / env / pid / ppid /
    uptime / exit / `setExitCode` / signal kill / `nextTick` /
    resource and memory usage / process title). Pure fields
    (`pid`, `ppid`, `platform`, `platformStr`, `version`,
    `debugPort`, the TTY-detection booleans) are re-exported
    as ordinary values. Event-handler bindings and the IPC
    `send` primitives are out of scope here.
  * `RIO.Node.Buffer` — RIO-flavoured wrappers around
    `Node.Buffer`. A `Buffer` is a mutable value rather than a
    capability, so every `Effect`-valued operation is lifted
    directly into `RIO` (`alloc` / `allocUnsafe` / `create` /
    `fromArray` / `fromString` / `fromArrayBuffer` /
    `toArrayBuffer` / `read` / `readString` / `toString` /
    `toString'` / `write` / `writeString` / `toArray` /
    `getAtOffset` / `setAtOffset` / `size` / `concat` /
    `concat'` / `copy` / `fill` / `freeze` / `unsafeFreeze` /
    `thaw` / `unsafeThaw` / `compareParts` / `poolSize` /
    `setPoolSize` / `swap16` / `swap32` / `swap64` /
    `transcode`). `slice` stays pure because it is a view, not
    an allocation.
  * `RIO.Node.EventEmitter` — RIO-flavoured wrappers around
    `Node.EventEmitter`. `new`, `getMaxListeners`,
    `listenerCount`, `setMaxListeners`,
    `setUnlimitedListeners`, and all four listener-add
    primitives (`on` / `once` / `prependListener` /
    `prependOnceListener` plus their `_`-suffix variants) are
    lifted into `RIO`. The removal callbacks returned by `on`,
    `once`, `prependListener`, and `prependOnceListener` come
    back as `RIO r e Unit` so they compose with the rest of an
    RIO program. `eventNames` and the built-in `newListenerH`
    and `removeListenerH` event handles are re-exported as
    pure values. `EventHandle`, `SymbolOrStr`, `JsSymbol`, and
    the `EventHandle0`..`EventHandle7` arity helpers are
    re-exported so callers do not need to import
    `Node.EventEmitter.*` directly.
  * `RIO.Node.ReadLine` — RIO-flavoured wrappers around
    `Node.ReadLine` and `Node.ReadLine.Aff`. `createInterface`,
    `createConsoleInterface`, `close`, `pause`, `resume`,
    `prompt` / `prompt'`, `setPrompt` / `getPrompt`,
    `writeData`, `writeKey`, `line`, `cursor`, `getCursorPos`,
    and the cursor / line / screen-control writers
    (`cursorToX[Y]` / `moveCursorXY` / `clearLineLeft` /
    `clearLineRight` / `clearEntireLine` / `clearScreenDown` /
    `emitKeyPressEvents`) all lift into `RIO`. `question` and
    `question'` are surfaced in their `Aff`-blocking form
    because the callback-style binding cannot run arbitrary
    `RIO r e` continuations from inside `Effect`. The
    `blockUntilClosed` and `countLines` `Aff` helpers are
    re-exposed as `RIO` actions, and all of the readline event
    handles (`closeH` / `lineH` / `historyH` / `pauseH` /
    `resumeH` / `sigContH` / `sigIntH` / `sigStpH`), the
    `InterfaceOptions` constructors, and `AbortController` are
    re-exported so callers do not need to reach for
    `Node.ReadLine.*` directly.
  * `RIO.Node.ChildProcess` — RIO-flavoured wrappers around
    `Node.ChildProcess` and `Node.ChildProcess.Aff`. The full
    process-launching surface is lifted into `RIO`: `spawn` /
    `spawn'`, `spawnSync` / `spawnSync'`, `exec` / `exec'`,
    `execSync` / `execSync'`, `execFile` / `execFile'`,
    `execFileSync` / `execFileSync'`, `fork` / `fork'`,
    `send` / `send'`, plus the lifecycle / introspection
    primitives (`pid`, `pidExists`, `connected`, `exitCode`,
    `signalCode`, `disconnect`, `kill` / `kill'`, `killSignal`,
    `killed`, `ref`, `unref`). The `Aff`-blocking
    `waitSpawned` is re-exposed as a `RIO` action. The
    `ForkOptions`, `SendOptions`, and `SpawnSyncResult` record
    aliases (which `Node.ChildProcess` keeps internal) are
    re-declared locally so callers can name them without
    reaching into the upstream module. The event handles
    (`closeH` / `disconnectH` / `errorH` / `exitH` /
    `messageH` / `spawnH`), the stream accessors (`stdin` /
    `stdout` / `stderr` / `stdio`), the `Exit` ADT and its
    constructors, and the assorted `Node.ChildProcess.Types`
    helpers (`Handle`, `KillSignal`, `Shell`, `StdIO`,
    `StringOrBuffer`, `UnsafeChildProcess`, plus the `pipe` /
    `inherit` / `ipc` / `ignore` / `overlapped` /
    `fileDescriptor` / `shareStream` / `customShell` /
    `enableShell` / `defaultStdIO` / `intSignal` /
    `stringSignal` / `fromKillSignal` / `fromKillSignal'`
    constructors) are re-exported so callers do not need to
    reach for `Node.ChildProcess.*` or `Node.ChildProcess.Types`
    directly.
  * `RIO.Node.Stream` — RIO-flavoured wrappers around
    `Node.Stream` and `Node.Stream.Aff`. A `Stream rw` is a value
    (a readable / writable / duplex handle) rather than a
    capability, so every `Effect`-valued primitive is lifted
    directly into `RIO`: `readable` / `readableEnded` /
    `readableFlowing` / `readableHighWaterMark` /
    `readableLength` / `resume` / `pause` / `isPaused` / `pipe` /
    `pipe'` / `unpipe` / `unpipeAll` / `read` / `read'` /
    `readString` / `readString'` / `readEither` / `readEither'` /
    `setEncoding`, the writable predicates (`writeable` /
    `writeableEnded` / `writeableCorked` / `errored` /
    `writeableFinished` / `writeableHighWaterMark` /
    `writeableLength` / `writeableNeedDrain`), the writers
    (`write` / `write'` / `writeString` / `writeString'`), the
    flush controls (`cork` / `uncork` / `setDefaultEncoding`),
    the termination primitives (`end` / `end'` / `destroy` /
    `destroy'` / `closed` / `destroyed`), `allowHalfOpen`,
    `pipeline`, and the constructors (`readableFromString` /
    `readableFromBuffer` / `newPassThrough`). The
    backpressure-aware `Aff`-blocking variants from
    `Node.Stream.Aff` are also re-exposed: `readableToStringUtf8`
    / `readableToString` / `readableToBuffers` / `readSome` /
    `readAll` / `readN` plus `toStringUTF8` / `fromStringUTF8`
    keep their original names, while the writer pair is
    surfaced as `writeAll` (`Array Buffer -> RIO Unit`, awaiting
    `drainH` between chunks) and `endAwait` (resolves once
    `finishH` has fired) so callers can opt into backpressure
    handling without colliding with the synchronous Effect-style
    `write` / `end`. The full event-handle catalogue
    (`closeH` / `errorH` / `drainH` / `finishH` / `pipeH` /
    `unpipeH` / `pauseH` / `readableH` / `resumeH` / `endH` /
    `dataH` / `dataHStr` / `dataHEither`) plus the `Stream` /
    `Readable` / `Writable` / `Duplex` / `Read` / `Write` /
    `Chunk` types and `toEventEmitter` are re-exported so
    callers do not need to reach for `Node.Stream` directly.
  * `RIO.Node.Net` — RIO-flavoured wrappers around `Node.Net`.
    A `Server`, `Socket`, `SocketAddress`, and `BlockList` are
    each values (handles on a listening TCP / IPC server, a TCP
    / IPC connection, an immutable address record, and a list of
    address rules respectively) rather than capabilities, so the
    upstream surface is mirrored by lifting each `Effect`-valued
    primitive into `RIO`. The top-level `RIO.Node.Net` module
    re-exports the pure `isIP` family and the full `Node.Net.Types`
    catalogue (`IpFamily`, `IPv4`, `IPv6`, `TCP`, `IPC`,
    `ConnectionType`, `Server`, `Socket`, `SocketAddress`,
    `BlockList`, `SocketReadyState`, and every option record).
    `RIO.Node.Net.SocketAddress` lifts the `newIpv4` / `newIpv6`
    constructors and re-exports the pure accessors. `RIO.Node.Net.BlockList`
    lifts the full mutator / query surface (`addAddressAddr` /
    `addAddressStr` / `addRangeAddrAddr` / `addRangeAddrStr` /
    `addRangeStrAddr` / `addRangeStrStr` / `addSubnetAddr` /
    `addSubnetStr` / `checkAddr` / `checkStr` / `rules`).
    `RIO.Node.Net.Socket` lifts the full socket surface (`newTcp`
    / `newIpc` / `createConnectionTCP` / `createConnectionIpc` /
    `connectTcp` / `connectIpc` / `connecting` / `destroySoon` /
    `address` / `bytesRead` / `bytesWritten` / `localAddress` /
    `localFamily` / `localPort` / `pending` / `ref` /
    `remoteAddress` / `remoteFamily` / `remotePort` /
    `resetAndDestroy` / `setKeepAlive` / `setKeepAliveBoolean` /
    `setKeepAliveInitialDelay` / `setKeepAliveAll` / `setNoDelay`
    / `setNoDelay'` / `setTimeout` / `clearTimeout` / `timeout` /
    `unref` / `readyState`) and re-exports the event handles
    (`closeH` / `connectH` / `lookupH` / `readyH` / `timeoutH`)
    plus `toDuplex` and `toEventEmitter`. `RIO.Node.Net.Server`
    lifts the full server surface (`createTcpServer` /
    `createTcpServer'` / `createIpcServer` / `createIpcServer'` /
    `addressTcp` / `addressIpc` / `close` / `getConnections` /
    `listenTcp` / `listenIpc` / `listening` / `maxConnections` /
    `ref` / `unref`) and re-exports the event handles (`closeH` /
    `connectionH` / `errorH` / `listeningH` / `dropHandleTcp` /
    `dropHandleIpc`) plus `toEventEmitter`.
  * `RIO.Node.HTTP` — RIO-flavoured wrappers around `Node.HTTP`.
    An `HttpServer` and `ClientRequest` are each values (handles
    on an HTTP server and an in-flight HTTP request respectively)
    rather than capabilities, so the upstream surface is mirrored
    across six modules by lifting each `Effect`-valued primitive
    into `RIO`. The top-level `RIO.Node.HTTP` module lifts
    `createServer` / `createServer'` / `request` / `requestUrl` /
    `request'` / `requestURL'` / `requestOpts` / `get` / `getUrl`
    / `get'` / `getUrl'` / `getOpts` / `setMaxIdleHttpParsers`
    and re-exports the `CreateServerOptions` / `RequestOptions`
    rows, `maxHeaderSize`, and the full `Node.HTTP.Types`
    catalogue (`HttpServer'`, `HttpServer`, `HttpsServer`,
    `Encrypted`, `PlainText`, `TransmissionType`, `ClientRequest`,
    `ServerResponse`, `OutgoingMessage`, `IncomingMessage`,
    `IncomingMessageType`, `IMServer`, `IMClientRequest`).
    `RIO.Node.HTTP.Server` lifts the server's introspection and
    timeout primitives (`closeAllConnections` /
    `closeIdleConnections` / `headersTimeout` /
    `setHeadersTimeout` / `maxHeadersCount` /
    `setMaxHeadersCount` / `setUnlimitedHeadersCount` /
    `requestTimeout` / `setRequestTimeout` /
    `maxRequestsPerSocket` / `setMaxRequestsPerSocket` /
    `setUnlimitedRequestsPerSocket` / `timeout` / `setTimeout` /
    `clearTimeout` / `keepAliveTimeout` / `setKeepAliveTimeout` /
    `clearKeepAliveTimeout`) and re-exports the event-handle
    catalogue (`checkContinueH` / `checkExpectationH` /
    `clientErrorH` / `closeH` / `connectH` / `connectionH` /
    `dropRequestH` / `requestH` / `upgradeH`), `toNetServer`,
    and the `ClientErrorException` / `bytesParsed` / `rawPacket`
    / `toError` triple. `RIO.Node.HTTP.IncomingMessage` lifts
    `complete` / `socket` / `trailers` / `trailersDistinct` and
    re-exports the pure accessors (`headers` / `headersDistinct`
    / `cookies` / `httpVersion` / `method` / `rawHeaders` /
    `rawTrailers` / `statusCode` / `statusMessage` / `url`) and
    `toReadable` / `closeH`. `RIO.Node.HTTP.OutgoingMessage`
    lifts the full header-manipulation surface (`addTrailers` /
    `appendHeader` / `appendHeaders` / `flushHeaders` /
    `getHeader` / `getHeaderNames` / `getHeaders` / `hasHeader`
    / `headersSent` / `removeHeader` / `setHeader` /
    `setHeader'` / `setTimeout` / `socket`) and re-exports
    `toWriteable` plus the `drainH` / `finishH` / `prefinishH`
    event handles. `RIO.Node.HTTP.ClientRequest` lifts
    `setNoDelay` / `setSocketKeepAlive` / `setTimeout` and
    re-exports the pure accessors (`host` / `method` / `path` /
    `protocol` / `reusedSocket`) plus the event-handle
    catalogue (`closeH` / `connectH` / `continueH` / `finishH`
    / `informationH` / `responseH` / `socketH` / `timeoutH` /
    `upgradeH`) and `toOutgoingMessage`. `RIO.Node.HTTP.ServerResponse`
    lifts the head-and-status primitives (`sendDate` /
    `setSendDate` / `statusCode` / `setStatusCode` /
    `statusMessage` / `setStatusMessage` / `strictContentLength`
    / `setStrictContentLength` / `writeEarlyHints` /
    `writeEarlyHints'` / `writeHead` / `writeHead'` /
    `writeHeadHeaders` / `writeHeadMsgHeaders` /
    `writeProcessing`) and re-exports `req` / `toOutgoingMessage`
    plus the `closeH` / `finishH` event handles. The
    `toTlsServer` conversion is intentionally omitted (it would
    pull in a direct dependency on `node-tls`; callers who need
    it can still reach for `Node.HTTP.Server.toTlsServer`
    directly).
  * `RIO.Node.HTTP2` — RIO-flavoured wrappers around `Node.Http2`.
    An `Http2SecureServer`, `Http2Session endpoint`, and
    `Http2Stream endpoint` are each values (handles on the
    underlying TLS / session / stream resources) rather than
    capabilities, so the upstream surface is mirrored across
    seven modules by lifting each `Effect`-valued primitive into
    `RIO`. The top-level `RIO.Node.HTTP2` module re-exports the
    most commonly used pure values from `Node.Http2.ErrorCode`
    (`ErrorCode` plus `cancel` / `compressionError` /
    `connectError` / `enhanceYourCalm` / `flowControlError` /
    `frameSizeError` / `http1_1Required` / `inadequateSecurity` /
    `internalError` / `noError` / `protocolError` /
    `refusedStream` / `settingsTimeout` / `streamClosed`),
    `Node.Http2.Flags` (`BitwiseFlag`, the per-frame `*Flags`
    bundles, the `ack` / `endHeaders` / `endStream` / `padded` /
    `priority` / `enable` flag values, `isEnabled` / `isDisabled`
    / `unFlag` / `printFlags` / `printFlags'`),
    `Node.Http2.FrameType` (`FrameType` plus the `frameData` /
    `frameHeaders` / `framePriority` / `frameRstStream` /
    `frameSettings` / `framePushPromise` / `framePing` /
    `frameGoAway` / `frameWindowUpdate` / `frameContinuation`
    constructors), `Node.Http2.PaddingStrategy` (the
    `PaddingStrategy` ADT only; the upstream
    `paddingStrategyMax` / `paddingStrategyNone` value bindings
    are not re-exported because they reference an unimported
    `constants` symbol in the upstream FFI and throw on module
    load), and the full `Node.Http2.Types` catalogue (`Headers`,
    `Http2ClientConnectOptions`,
    `Http2CreateSecureServerOptions`, `Http2SecureServer`,
    `Http2ServerRequest`, `Http2ServerResponse`, `Http2Session`,
    `Http2Stream`, `Settings`, `StreamId`, `connectionId`).
    `RIO.Node.HTTP2.Headers` re-exposes the pure helpers
    (`mkHeadersI` / `mkHeaders` / `method` / `path` / `scheme` /
    `authority` / `lookup` / `status` / `printHeaders` /
    `printHeaders'` / `unsafeToObject`).
    `RIO.Node.HTTP2.Settings` lifts `getDefaultSettings` /
    `getPackedSettings` / `getUnpackedSettings` into `RIO` and
    re-exports the pure `defaultSettings` record. (The upstream
    `getDefaultSettings` / `getPackedSettings` /
    `getUnpackedSettings` FFI bindings throw at call time because
    the upstream `Node.Http2.Settings.js` references an
    unimported `http2` symbol; the wrappers are still surfaced so
    callers can drive them once the upstream package is fixed.)
    `RIO.Node.HTTP2.Session` lifts the full session surface
    (`alpnProtocol` / `close` / `closed` / `connecting` /
    `destroy` / `destroyWithError` / `destroyWithCode` /
    `destroyWithErrorCode` / `destroyed` / `encrypted` / `goAway`
    / `goAwayCode` / `goAwayCodeLastStreamId` /
    `goAwayCodeLastStreamIdData` / `localSettings` /
    `pendingSettingsAck` / `ping` / `pingPayload` / `ref` /
    `remoteSettings` / `setLocalWindowSize` / `setTimeout` /
    `settings` / `socket` / `state` / `unref` / `altsvcStreamId`
    / `altsvcOrigin` / `origin` / `request` / `request'`) and
    re-exports the pure `originSet` / `type_` accessors, the
    `Http2SessionState` / `RequestOptions` row aliases, and the
    full event-handle catalogue. `RIO.Node.HTTP2.Stream` lifts
    the full stream surface (`bufferSize` / `close` / `closed` /
    `destroyed` / `endAfterHeaders` / `id` / `pending` /
    `priority` / `rstCode` / `sentHeaders` / `sentInfoHeaders` /
    `sentTrailers` / `session` / `setTimeout` / `state` /
    `sendTrailers` / `additionalHeaders` / `headersSent` /
    `pushAllowed` / `pushStream` / `pushStream'` / `respond` /
    `respondWithFd` / `respondWithFile`) and re-exports
    `toDuplex` plus the event-handle catalogue.
    `RIO.Node.HTTP2.Server` lifts `createSecureServer` (carrying
    the stacked TLS `Row.Union` constraint from
    `Http2CreateSecureServerOptions`), `setTimeout`, `timeout`,
    and `updateSettings`, and re-exports the server-level event
    handles (`checkContinueH` / `requestH` / `sessionErrorH` /
    `sessionH` / `streamH` / `timeoutH` / `unknownProtocolH`)
    plus the `toTlsServer` raw conversion and the `toNetServer`
    convenience (which composes `toTlsServer` with
    `Node.TLS.Server.toTcpServer` so callers can reach the
    underlying TCP listener without importing `Node.TLS.Server`
    or `Node.Http2.Server` directly).
    `RIO.Node.HTTP2.Client` lifts `connect` / `connect'` (the
    latter carrying the stacked TLS + TCP `Row.Union` constraint
    from `Http2ClientConnectOptions`). `node-tls` is added to the
    package's main dependencies for the TLS option rows that
    feed `createSecureServer` and `connect'`. A self-signed
    `localhost` certificate fixture and a secure-server / client
    round-trip test (server speaks raw-stream mode via `streamH`
    and `respond`, client opens a request stream and reads the
    response body) covers the end-to-end path through the
    `createSecureServer` / `streamH` / `respond` / `toNetServer` /
    `connect'` / `request` / `toDuplex` surface.
  CI builds and tests the new package alongside the existing
  adapters.
- `RIO.Test.Property`: a thin RIO-tuned property harness exposing
  `forAllRIO`, `forAllRION`, `defaultSampleCount`, and
  `generateSamples`. Property specs in this repo had each redefined
  the same hand-rolled `forAll :: Gen a -> (a -> Aff Unit) -> Aff Unit`
  helper that calls `randomSample'` and `for_`; the new module
  standardises that pattern with a `MonadEffect`-polymorphic
  signature so the same harness works for `Aff`-shaped specs and
  for RIO programs (without forcing the latter through `runRIO`
  at the property boundary). No shrinking, by design (the
  PureScript QuickCheck port does not carry an integrated
  shrinker); on failure the harness reports the first
  counter-example with the body's own assertion. Whole-package
  run goes `1081 -> 1091` tests passing.
- `RIO.Error`: row-list-keyed payload lookup for `catchTag`. The new
  `FindErrorTag` / `FindErrorTagInRow` / `CatchableErrorTag` classes
  walk an error row's `RowList` to determine the handler's payload
  type from the tag's symbol. A wrong-typed handler now surfaces a
  clean "Could not match type X with type Y" error pointed at the
  two payload types directly rather than a `Prim.Row.Cons`
  row-mismatch over the whole error row. Promotes the `compile-fail`
  case 03 ("`catchTag` with a wrong payload type") from
  ACCEPTABLE-NOISY to GOOD; case 04 ("tag not in row") remains
  ACCEPTABLE-NOISY because the `Prim.Row.Cons` constraint still
  needed for the residual-row calculation fires first at the use
  site.
- `RIO.Stream.Resource`: one additional unit test pinning that
  `bracketStream` releases the acquired resource on the
  fiber-kill termination path. The module docstring promises
  release on "every termination path (success, typed failure,
  defect, or fiber kill)"; success, typed-failure, and defect
  were pinned. Pin the fiber-kill path so the full bracket
  contract is documented through the user-facing
  `bracketStream` surface. Whole-package run goes
  `444 -> 445` tests passing.
- `RIO.Semaphore`: one additional unit test pinning that a fiber
  killed while parked on `withPermit` removes itself from the
  waiters list cleanly. The source comment on `acquire` promises
  this cleanup, but the existing tests only killed a parked
  waiter for cleanup without observing it. Pin the contract by
  parking a waiter behind a held permit, killing it, releasing
  the permit, and asserting that `available` returns to 1 (a
  stale entry would silently consume the permit during `drain`).
  Whole-package run goes `443 -> 444` tests passing.
- `RIO.Resource.Do`: two additional unit tests pinning that a
  multi-acquire `Resource.do` block releases in LIFO order on the
  defect and fiber-kill termination paths. The module docstring
  promises that "every release [runs] on every termination path
  (success, typed failure, defect, kill)" in LIFO order. Success,
  typed failure, and acquire-failure were pinned; pin the defect
  and kill paths on the qualified-do surface so the whole
  contract is documented through `Resource.do` (not only through
  the lower-level `acquireRelease`). Whole-package run goes
  `441 -> 443` tests passing.
- `RIO.STM.THub`: two additional unit tests pinning that
  `withSubscription` releases the subscription on the defect and
  fiber-kill termination paths. The docstring promises release
  "on every termination path of `use` (success, typed failure,
  defect, interrupt)". Success and typed-failure were pinned;
  pin the remaining two so the full bracket contract is
  documented across all four paths. Whole-package run goes
  `439 -> 441` tests passing.
- `RIO.Stream.Par`: one additional unit test pinning that
  `mergeAll` propagates a defect raised inside a producer. The
  module-level docstring promises a single failure model for
  every combinator: "the first typed failure or defect observed
  in any producer shuts the shared queue down". Typed failure
  was pinned for `mergeAll`, `mergeMap`, `broadcast`, and
  `partition`; pin the defect path on `mergeAll` so the full
  failure contract is documented. Whole-package run goes
  `438 -> 439` tests passing.
- `RIO.Layer`: one additional unit test pinning that finalizers
  registered by two horizontally-composed layers fire LIFO when
  the surrounding scope exits. The `combine` docstring promises
  "Both layers run in the same surrounding scope; their
  finalizers join the scope's stack and fire LIFO on exit". The
  existing tests covered horizontal output-row union and
  short-circuit-on-failure but not the LIFO ordering. Pin it by
  registering one finalizer per layer in a two-layer `combine`
  driven through `buildLayer` and asserting the second layer's
  finalizer fires first. Whole-package run goes `437 -> 438`
  tests passing.
- `RIO.Queue`: one additional unit test pinning that a killed
  offerer removes itself from the bounded queue's offerers list.
  Symmetric to the taker-cleanup contract just pinned: a bounded
  queue parks producers when at capacity, and the Canceler
  registered by `offer` must remove the producer on kill;
  otherwise a later `take` would resume a dead offerer (no-op)
  and the parked value would silently land in `items`. Pin the
  cleanup by killing a parked offerer and observing the queue
  only carries the one live item. Whole-package run goes
  `436 -> 437` tests passing.
- `RIO.Queue`: one additional unit test pinning that a killed
  taker removes itself from the queue's takers list. The module
  docstring promises "the list of blocked takers (so an
  interrupted taker can remove itself cleanly)". If the
  registered canceler did not run on kill, a later `offer` would
  try to deliver to the dead taker (whose `resume` is a no-op)
  and the value would be lost; the subsequent `take` would block
  forever. Pin the cleanup by killing a blocked taker, offering
  a value, and observing that a fresh `take` retrieves it.
  Whole-package run goes `435 -> 436` tests passing.
- `RIO.Concurrency`: one additional unit test pinning that
  `parTraverseN`'s cross-chunk short-circuit holds. The
  `parTraverseN` docstring promises "the first typed failure
  inside a chunk cancels its siblings and aborts the remaining
  chunks". Existing tests pin order preservation, the concurrency
  cap, and the `n <= 1` sequential degenerate; pin the
  remaining-chunk abort by failing on item `2` under
  `parTraverseN 2` over `[1..6]` and asserting that no item from
  `[3,4]` or `[5,6]` ever started. Whole-package run goes
  `434 -> 435` tests passing.
- `RIO.Concurrency`: one additional unit test pinning that a
  queued interrupt fires once the `uninterruptible` region exits.
  The `uninterruptible` docstring promises "any `interrupt` sent
  to the enclosing fiber is queued; it fires only after the
  region completes". Existing tests pin that the protected
  section runs to completion despite an interrupt, but not the
  second half: that the queued interrupt lands at the region
  boundary. Add a post-region statement to the child and assert
  it never runs. Whole-package run goes `433 -> 434` tests
  passing.
- `RIO.Schedule`: one additional unit test pinning that `retry`
  does not retry on a defect. The `retry` docstring promises
  "Defects (from `die` or any uncaught `Aff` exception) skip
  retry and propagate immediately; sandbox the action if you
  want a defect to feed back into the schedule". Existing tests
  pin transient typed-failure recovery and final-failure surface
  but not the defect short-circuit. Pin it by retrying an action
  that `die`s on every call under `recurs 5` and asserting it
  ran exactly once and the defect surfaced through `attempt`.
  Whole-package run goes `432 -> 433` tests passing.
- `RIO.Logger`: two additional unit tests pinning the remaining
  termination paths of `withFields`'s restore-on-exit bracket.
  The docstring promises restoration "by `Aff.finally` on every
  termination path (success, typed failure, defect, fiber
  interruption)". Success and typed-failure were pinned; pin the
  defect and fiber-kill paths so the full bracket contract is
  documented across all four termination paths. Whole-package
  run goes `430 -> 432` tests passing.
- `RIO.Stream.Resource`: one additional unit test pinning that
  `bracketStream`'s registered finalizer fires on the defect
  termination path. The module docstring promises release "on
  every termination path (success, typed failure, defect, or
  fiber kill)". Success and typed-failure were pinned; pin the
  defect path so the full bracket contract is documented for
  resource-owning streams. Whole-package run goes `429 -> 430`
  tests passing.
- `RIO.Layer`: one additional unit test pinning that finalizers
  registered by two sequentially-composed layers fire LIFO when
  the surrounding scope exits. The `andThen` docstring promises
  "Both layers run in the same surrounding scope, so finalizers
  from either fire (in LIFO order) when that scope exits". The
  existing spec covered sequential composition's data flow and
  short-circuit-on-failure but did not pin the LIFO ordering of
  the shared finalizer stack. Pin it by registering one finalizer
  per layer in a two-layer `andThen` driven through `buildLayer`
  and asserting the second layer's finalizer fires first.
  Whole-package run goes `428 -> 429` tests passing.
- `RIO.Concurrency.Par`: one additional unit test pinning the
  module's defect-propagation contract. The module docstring
  promises "A defect (`Aff` exception) in any branch
  propagates; the other branches are interrupted by the
  underlying `ParAff` runtime". The existing spec covered
  parallel timing, leftmost typed-failure bias, no-short-
  circuit on typed failures, and right-branch failure surfacing,
  but did not pin defect handling. Pin both halves: a defect
  raised by one branch surfaces as an `Aff` exception
  observable through `attempt`, and the sibling branch's
  post-delay side effect never lands because `ParAff` cancels
  it. Whole-package run goes `427 -> 428` tests passing.
- `RIO.Test.Clock`: one additional unit test pinning that
  `sleep (Milliseconds 0.0)` returns immediately, without
  waiting on an `advance`. The test clock's `sleep` takes a
  short-circuit branch when `deadlineMs <= current`, resuming
  the sleeper without parking it on the pending list. Pin
  that branch so a future refactor that uniformly parks every
  sleeper (and silently hangs zero-duration sleepers until
  the next `advance`) is caught. Whole-package run goes
  `426 -> 427` tests passing.
- `RIO.Local`: two additional unit tests pinning the
  remaining termination paths of `locally`'s restore-on-exit
  bracket. The docstring promises the previous value is
  restored "regardless of how it terminates (success, typed
  failure, defect, or interrupt)". Success and typed-failure
  were pinned; pin the defect and fiber-kill paths so the full
  bracket contract is documented across all four termination
  paths. Whole-package run goes `424 -> 426` tests passing.
- `RIO.Config`: one additional unit test pinning that
  `combine` flattens `Multi` so accumulated errors stay at one
  level. The `combine` docstring promises "Flattens `Multi`
  so nesting stays shallow regardless of how the descriptor
  tree was assembled". The existing two-failure test cannot
  distinguish a flat `Multi [a, b]` from any alternative
  two-element layout. Pin the flatten promise with three
  independent failures from a record-shaped descriptor (three
  missing keys) and assert the resulting `Multi` carries
  exactly three children at the top level. Whole-package run
  goes `423 -> 424` tests passing.
- `RIO.Hub`: one additional unit test pinning that a slow
  consumer does not block publishes or other subscribers.
  The module's docstring promises "a slow consumer does not
  slow the producer down" with the natural tradeoff that "a
  slow consumer can fall arbitrarily far behind" because each
  subscriber holds its own unbounded queue. Pin both halves
  by leaving one subscriber undrained while publishing a
  batch synchronously, draining the fast subscriber first
  (must see every value in order without blocking), then
  draining the slow subscriber (must hold every value in
  order). Whole-package run goes `422 -> 423` tests passing.
- `RIO.Tracer.OTel`: one additional unit test pinning
  `addAttribute` safety on an already-closed `SpanId`. The
  spec module's docstring promises "addAttribute safety
  against already-closed or unknown span ids"; the unknown
  case was already pinned, but the already-closed case was
  only covered indirectly through `endSpan` idempotence. Pin
  it directly so a future change that retains closed spans in
  the internal map and forwards attribute writes to a
  finalized OTel span (which would throw at runtime) is
  caught. rio-otel package run goes `12 -> 13` tests passing.
- `RIO.Error`: one additional unit test pinning `rethrow`'s
  direct behaviour. `rethrow` is the "dual of `fail`" at the
  Variant level: given an already-constructed `Variant e`, it
  wraps the variant back into a `Left` on the same row. The
  existing coverage only exercised `rethrow` through the
  `catchAll rethrow ≡ identity` composition, which can be
  satisfied by other equivalent implementations. Pin the raw
  contract by constructing a `Variant (boom :: String)`,
  passing it directly to `rethrow`, and asserting `runRIO`
  surfaces the exact tagged failure. Whole-package run goes
  `421 -> 422` tests passing.
- `RIO.Semaphore`: one additional unit test pinning that
  `withPermit` releases the permit after a fiber kill
  mid-action. `withPermits` wires release through
  `Effect.Aff.finally`, which is documented to fire on every
  termination path; the typed-failure and defect paths were
  already pinned. Pin the kill case by forking an `Aff` that
  runs a `withPermit` action which sleeps, killing the fiber
  mid-action, and asserting the permit count returns to its
  initial value. Whole-package run goes `420 -> 421` tests
  passing.
- `RIO.STM`: one additional unit test pinning that `orElse`
  rolls back staged writes from a retried left branch. The
  docstring promises "the log effect of a fallen-through
  `left` is rolled back before `right` runs, so a retried
  branch leaves no reads or writes behind." The fall-through
  path was pinned; this pins the rollback specifically by
  staging a write inside the retrying left, falling through
  to the right, and asserting the staged write did not commit
  to the surrounding atomic block. Whole-package run goes
  `419 -> 420` tests passing.
- `RIO.Tracer`: one additional unit test pinning the `Show
  SpanStatus` instance. The docstring distinguishes three
  terminal outcomes (`SpanOk`, `SpanFailed`,
  `SpanInterrupted`); pin that `show` renders each by its
  constructor name so any log or OTel exporter that uses
  `show status` to label a status code can't be silently
  broken. Whole-package run goes `418 -> 419` tests passing.
- `RIO.Schedule`: two additional unit tests pinning the
  documented output and delay contracts of `intersect`. The
  existing test only pinned the "stops as soon as either side
  stops" promise. The docstring also promises that (a) the
  output is the tuple of per-schedule outputs, and (b) the
  delay is the larger of the two so both schedules can keep
  up. Add (a) a tuple-output test running `intersect (recurs
  3) (recurs 3)` and asserting `[Tuple 1 1, Tuple 2 2, Tuple 3
  3]`; and (b) a max-delay test pairing a 50ms spaced schedule
  with a 200ms one and asserting the emitted delays are all
  200ms. Whole-package run goes `416 -> 418` tests passing.
- `RIO.Logger`: two additional unit tests pinning the `Show`
  and derived `Ord` instances on `LogLevel`. The docstring
  promises a five-band order from `LogTrace` (noisiest) to
  `LogError` (loudest non-defect signal). The new tests pin
  (a) `Show` renders each constructor by its name and (b) the
  derived `Ord` follows the documented declaration order, so
  any log pipeline that relies on either instance can't be
  silently broken. Whole-package run goes `414 -> 416` tests
  passing.
- `RIO.STM.TSemaphore`: one additional unit test pinning that
  `withTSemaphore` releases the permit after a fiber kill. The
  docstring promises release on every termination path
  ("success, typed failure, defect, kill"). The first three
  paths were already pinned; the kill case was not. The new
  test forks an `Aff` running a `withTSemaphore` action that
  sleeps, kills the fiber mid-action, and asserts the permit
  count returns to its initial value, so the full bracket
  contract is documented. Whole-package run goes `413 -> 414`
  tests passing.
- `RIO.Core`: one additional unit test pinning that `unsafeRunRIO`
  surfaces a typed failure on the `Left` branch of its
  `Aff (Either (Variant e) a)` result. The existing test only
  pinned the `Right` (success) branch with a populated
  environment, so the third runner's typed-error surface was
  undocumented next to `runRIO`'s. The new test runs a `fail`
  through `unsafeRunRIO` and inspects the resulting `Variant`
  by tag, pinning that the typed channel is preserved through
  this lower-level runner. Whole-package run goes `412 -> 413`
  tests passing.
- `RIO.Concurrency`: one additional unit test pinning that a
  typed failure raised inside `uninterruptible` surfaces on the
  parent's row. The existing test pins that the block completes
  despite an interrupt; the failure path was not pinned. The
  new test exercises a typed failure inside the protected
  region and asserts it propagates as a `Left`, since
  `uninterruptible` only blocks kills, not the typed-error
  channel. Whole-package run goes `411 -> 412` tests passing.
- `RIO.Local`: one additional unit test pinning `newLocalEffect`,
  the `Effect`-typed escape hatch for callers that build their
  environment record outside an `RIO` action. The previous
  Local suite only used `newLocal` (RIO-typed). The new test
  allocates a `Local` in `Effect`, then exercises `get` and
  `update` from inside an `RIO` program, asserting the same
  semantics as `newLocal`. Whole-package run goes `410 -> 411`
  tests passing.
- `rio-config-file`: two additional unit tests in `flattenJson`
  for top-level shape handling. The previous coverage rejected
  `42` and a string but never pinned the rest of the non-object
  surface, nor the documented `null` exception. Add (a) a test
  that rejects top-level `true`, `false`, and an array; and (b)
  a test that pins the docstring promise that a top-level
  `null` returns an empty map (so the "must be an object"
  rejection is not silently widened to include `null`).
  Sub-package run goes `31 -> 33` tests passing.
- `RIO.Config`: two additional unit tests for the `boolean`
  primitive. The previous test only covered `"yes"` as a single
  truthy value, but the docstring promises a much wider
  contract: `true`/`false`, `yes`/`no`, `on`/`off`, `1`/`0`,
  case-insensitive. Add (a) an exhaustive synonym test covering
  all documented forms plus a mixed-case check; and (b) a
  rejection test pinning that a non-synonym value produces a
  `ParseError`. Catches any silent narrowing of the accepted
  set. Whole-package run goes `408 -> 410` tests passing.
- `RIO.Config`: two additional unit tests for `nested`. The
  existing test only showed the one-level form (`nested "DB"
  (string "URL")` reads `DB_URL`). Add (a) a composition test
  pinning two stacked `nested` layers produce an
  `OUTER_INNER_K` key, so any future refactor that joins the
  path differently is caught; and (b) a failure-path test
  pinning the namespace is carried into a `MissingKey` error's
  `path` array so `prettyConfigError` can render `DB.URL`.
  Whole-package run goes `406 -> 408` tests passing.
- `RIO.Config`: two additional unit tests pinning `mkSource`, the
  general escape-hatch source constructor. The whole config suite
  was using `mapSource` to build sources for tests, leaving the
  `String -> Maybe String` constructor with no direct coverage.
  The new tests pin that (a) a `Config` descriptor reads
  successfully through a custom lookup function (parsing
  `int "PORT"` from a closure returning `Just "9090"`), and (b)
  the descriptor returns a `MissingKey` error when the lookup
  returns `Nothing`. Whole-package run goes `404 -> 406` tests
  passing.
- `RIO.Hub`: one additional unit test pinning the `unsubscribe`
  smart constructor's docstring promise that it is "Equivalent
  to running that action directly; provided for readability."
  The existing tests all call `sub.unsubscribe` (the action
  inside the returned record) but never exercise the exported
  `unsubscribe` wrapper. The new test runs `unsubscribe
  sub.unsubscribe`, asserts the subscriber count returns to
  zero, and asserts a subsequent `publish` is not delivered
  to the removed subscriber's queue. Whole-package run goes
  `403 -> 404` tests passing.
- `RIO.Tracer`: one additional unit test pinning the `SpanInterrupted`
  status, the only `SpanStatus` constructor with no direct test
  coverage. The docstring promises that "SpanInterrupted means the
  fiber was killed before the action completed". The new test
  forks a `withSpan` containing an `Aff.delay`, kills the fiber
  before the delay elapses, and asserts the recorded span closes
  with `SpanInterrupted` and a populated `endMs`, confirming the
  `Aff.finally` cleanup path inside `withSpan` actually runs and
  records the interruption. Whole-package run goes `402 -> 403`
  tests passing.
- `RIO.Schedule`: one additional unit test pinning `exponential`'s
  emitted delay values directly. The existing "exponential under
  the test clock" test asserts step-firing cadence indirectly via
  a Ref counter after `tc.advance` calls, but does not pin the
  schedule's emitted `Milliseconds` values themselves. The new
  test uses the existing `collectDelays` helper to assert that
  `exponential (Milliseconds 100.0) 2.0` emits `[100, 200, 400,
  800]` for its first four steps, so any change to the growth
  formula (or accidental swap of base/factor semantics) is
  caught directly. Whole-package run goes `401 -> 402` tests
  passing.
- `RIO.Schedule`: one additional unit test pinning the docstring
  promise that `retryOrElse` runs the fallback immediately when
  the schedule's first step is `Done` (no retry allowed). Uses
  `recurs 0` and a Ref-tracked action call counter to assert
  the action ran exactly once before the fallback was invoked.
  Whole-package run goes `400 -> 401` tests passing.
- `RIO.Tracer`: three additional unit tests covering `noopTracer`,
  the discarding tracer constructor that had no direct coverage.
  The first runs `withSpan` (nested), `addAttribute`, and the
  current-span read through `noopTracer` and asserts nothing
  crashes. The second pins that `currentSpan` reports `Nothing`
  even inside a `withSpan` block, since `noopTracer` does no
  bookkeeping. The third pins that a typed failure raised
  inside a `withSpan` body still propagates on the parent's row
  even under `noopTracer`'s no-op span lifecycle. Whole-package
  run goes `397 -> 400` tests passing.
- `rio-config-file`: one additional unit test in `flattenJson`
  pinning the number-rendering contract directly. The
  "flattens nested objects" test already shows the integer
  case incidentally (an `8` flattens to `"8"`), but the suite
  had no dedicated test pinning what the flattener does to
  numbers across an integer, a fractional, and a negative.
  The new test pins all three so any silent switch of
  stringifier (or accidental forced `.0` suffix) is caught.
  Sub-package run goes `30 -> 31` tests passing.
- `RIO.Sink`: one additional unit test pinning `Sink.foldM`'s
  typed-failure surface. The Sink suite had no failure-path
  coverage at all; every existing test exercised either pure
  combinators or effectful steps that never raised. The new
  test raises a typed failure from inside a `foldM` step on
  the third element of a five-element source and pins both
  that the failure surfaces on the parent's row and that the
  step was invoked exactly three times (so the surrounding
  pipeline did stop pulling rather than continue past the
  failing call). Whole-package run goes `396 -> 397` tests
  passing.
- `RIO.Stream`: two additional unit tests pinning typed-failure
  propagation through the pull-based pipeline. The existing
  suite covered happy-path accumulation through `mapM` and
  `runFoldM` but never exercised what happens when their
  effectful step raises a typed failure. The new tests assert
  that a `mapM` step failure surfaces on the parent's row and
  short-circuits the pipeline (later elements are never
  visited), and the analogous contract for `runFoldM`'s
  effectful accumulator. Both tests use a `Ref`-based visit
  log to pin the exact prefix of elements that were stepped
  before the failure propagated. Whole-package run goes
  `394 -> 396` tests passing.
- `RIO.Schedule`: two additional unit tests pinning `spaced`
  directly. The combinator was previously exercised only as
  the inner schedule for `jittered` and indirectly via
  `forever = spaced 0.0`, leaving its own surface untested.
  The new tests pin the docstring promise that `spaced ms`
  emits the supplied delay verbatim at every step, and that
  its output is an iteration counter starting at 1. Whole-
  package run goes `392 -> 394` tests passing.
- `RIO.Stream.Par`: one additional unit test pinning `mergeMap`'s
  typed-failure propagation. The module-level docstring states
  that every combinator in `RIO.Stream.Par` shares the same
  failure model (the first typed failure or defect in any
  producer shuts the shared queue down), and the suite already
  pins this for `mergeAll`, `broadcast`, and `partition`. The
  new test pins the same contract for `mergeMap` by raising a
  typed failure inside one of the inner streams produced by the
  fan-out function and asserting it surfaces on the parent's
  row. Whole-package run goes `391 -> 392` tests passing.
- `RIO.STM.THub`: three additional unit tests filling concrete gaps
  in the THub suite. `isEmptySubscription` was exported but was not
  imported by, or used in, any existing test; two new tests pin its
  behaviour on a fresh subscription (true) and across the
  publish/drain cycle (false while buffered, true again after every
  buffered value is taken). A third test extends the `withSubscription`
  coverage from the happy path to the typed-failure path, pinning
  the docstring promise that the subscription is released "on every
  termination path of `use`" by asserting `subscriberCount` drops
  back to zero after a typed failure surfaces through the block.
  Whole-package run goes `388 -> 391` tests passing.
- `RIO.Config`: four additional unit tests pinning the rendering
  contract of `prettyConfigError`, the user-facing pretty-printer
  for config-load failures. The previous suite exercised the
  loader paths thoroughly but never asserted what the rendered
  text looks like, so any silent change in the message format
  would have slipped through. The new tests cover all three
  `ConfigError` constructors: `MissingKey` with and without a
  namespace path (verifying the dotted `DB.URL` form), a
  `ParseError` (verifying the `key: message` shape), and a
  `Multi` (verifying the "config failed to load:" header plus
  one indented bullet per child error). Whole-package run goes
  `384 -> 388` tests passing.
- `RIO.Config.Rotating`: two additional unit tests pinning the
  `withRotation` docstring contract about loader failure on a
  subsequent refresh. The first asserts that the failure
  propagates from `refresh` on the chosen error row (the
  pre-existing tests only exercised the happy refresh path).
  The second asserts the cell keeps its last successful value
  when a refresh fails: build a cell whose loader succeeds on
  call 1 and fails on call 2, call refresh, observe that the
  cell still reads back the initial value. Whole-package run
  goes `382 -> 384` tests passing.
- `RIO.Logger`: two additional unit tests covering `noopLogger`,
  the discarding logger constructor previously without direct
  coverage. The first runs every level smart constructor
  (`logTrace` through `logError`) through the noop logger and
  asserts it completes without crashing. The second exercises
  `withField` + a typed failure inside its body, pinning the
  module-level docstring promise that `noopLogger` still
  cycles annotation state correctly even when emissions are
  discarded. Whole-package run goes `380 -> 382` tests passing.
- `RIO.Concurrency.Par`: two additional unit tests pinning the
  module's documented semantics. The first verifies the
  "no short-circuit" promise: when the left branch raises a
  typed failure, the right branch still runs every one of its
  internal steps to completion (observable via a Ref counter).
  The second verifies that a right-side typed failure surfaces
  on the parent's row when the left branch succeeds (the
  pre-existing test only exercised the leftmost-failure-wins
  case). Whole-package run goes `378 -> 380` tests passing.
- `RIO.Stream`: one additional unit test for `repeatM`
  (an exported function with no test). The new case pairs
  `repeatM` with `take 4`, asserting that the inner action
  is invoked exactly 4 times and the produced array has the
  expected elements. Every exported Stream symbol now has
  at least one direct test. Whole-package run goes
  `377 -> 378` tests passing.
- `RIO.Schedule`: six additional unit tests covering five
  previously-untested exported combinators: `once`,
  `forever`, `mapSchedule`, `andThen`, and `whileInput`
  (two cases for the last, one for predicate-false and one
  for predicate-true delegation). Together with the existing
  coverage of `recurs`, `spaced`, `exponential`, `intersect`,
  `jittered`, `repeat`, `retry`, `retryOrElse`, and `step`,
  every exported Schedule symbol now has at least one direct
  test. Whole-package run goes `371 -> 377` tests passing.
- `RIO.STM`: two additional `orElse` unit tests filling in the
  documented behaviour matrix. The first pins the left-commits
  happy path (left side returns a value, right side is not
  consulted); the second pins the both-sides-retry case (outer
  transaction itself retries and wakes when a TRef the right
  side read is written by another fiber). Combined with the
  existing two cases (left-retries-falls-through, left-typed-
  failure-does-not-fall-through) the four-cell behaviour matrix
  is now covered. Whole-package run goes `369 -> 371` tests
  passing.
- `RIO.STM.TQueue`: two additional unit tests for `peekTQueue`
  (an exported function with no test). The first pins the
  non-destructive read behaviour (queue length unchanged after a
  peek, subsequent readTQueue returns the same head); the second
  exercises the retry path (peek on empty blocks until a producer
  writes). Whole-package run goes `367 -> 369` tests passing.
- `RIO.STM.TMap` and `RIO.STM.TSemaphore`: nine additional unit
  tests covering coverage gaps. TMap picks up missing-key
  lookup, insert-overwrite, delete-of-absent-key no-op, empty-
  map size, and awaitKey-with-key-already-present (no retry);
  TSemaphore picks up `acquireN` / `releaseN` (the multi-permit
  smart constructors, previously only exercised through the
  `withTSemaphore`/`acquireTSemaphore` aliases), incremental
  `availableTSemaphore` tracking, and the release-on-typed-
  failure / release-on-defect contracts (mirrors the non-STM
  `RIO.Semaphore` tests added earlier this session).
  Whole-package run goes `358 -> 367` tests passing.
- `docs/01-core-type.md`, `docs/02-services.md`, and
  `docs/13-streams.md` now have the `## Pointers` section every
  other 01-15 numbered reference doc carries. The three were the
  only outliers (01 and 02 predate the pattern; 13 had a
  `Worked examples` section but no source / spec links). Each
  new block points at the relevant `.purs` source, the matching
  spec file, and related docs (e.g. 01 -> 02 + 03; 02 -> 04;
  13 -> 06 + `sink-design.md`).
- `RIO.Queue`: five additional unit tests filling in gaps around
  `size`, `poll` on a non-empty queue, and the post-shutdown
  semantics that the module docstring promises. New tests pin
  the size-reflects-offer/take behaviour, poll-removes-the-item
  contract, take-drains-buffered-then-Nothing behaviour after
  shutdown, offer-returns-false-after-shutdown, and (bounded
  case) shutdown-wakes-blocked-offerers-with-false. Whole-package
  run goes `353 -> 358` tests passing.
- `RIO.Hub`: four additional unit tests covering coverage gaps.
  `publishAll` (an exported function with no test) now has a
  batch-order assertion across two subscribers; the
  `make`-starts-empty contract is pinned; publishing to a hub
  with zero subscribers is shown to be a no-op (and not silently
  buffered for a later `subscribe`); and the unsubscribe path is
  exercised across a sequence of publishes to confirm the
  unsubscribed consumer stops receiving while peers continue.
  Hub describe count goes `4 -> 8` and the whole-package run
  goes `349 -> 353` tests passing.
- `RIO.Deferred`: five additional unit tests covering the
  write-once boundary between succeed and fail, the polled-after-
  fill content for both kinds, and the multi-awaiter wake on a
  typed failure. Deferred describe count goes `5 -> 10` and the
  whole-package run goes `344 -> 349` tests passing. Confirms the
  module's docstring promise that "once filled it stays filled"
  works in both orders (succeed then fail loses, fail then succeed
  loses).
- `RIO.Metrics` and `RIO.Semaphore`: thirteen additional unit tests
  covering previously uncovered behaviour. `MetricsSpec` picks up
  `recordGauge` / `recordHistogram` called directly (not just via
  their `setGauge` / `observeHistogram` aliases), distinct
  counter-name independence, same-name-across-kinds non-collision,
  negative gauge values, the snapshot-after-typed-failure
  ordering guarantee, the `noopMetrics` no-emission contract, and
  the empty-program no-emission contract; `npx spago test -p rio`
  on the metrics describe now reports `10/10` (up from `2/2`).
  `SemaphoreSpec` picks up the release-on-typed-failure,
  release-on-defect, and release-on-typed-failure-while-holding-N
  paths that the module's docstring promises but no test pinned;
  plus `withPermits 0` non-blocking semantics and 0-permit
  blocking, bringing semaphore coverage from `4` tests to `9`.
  Whole-package run goes `331 -> 344` tests passing.
- `PROJECT_BUILD_PLAN.md`: refreshed two stale entries. The
  rio-postgres bullet under "Items from the previous plan that
  have landed" no longer claims Postgres integration tests are
  "still open"; it now describes the actual test layout under
  `rio-postgres/test/`, `rio-postgres-json/test/`, and
  `rio-postgres-migrate/test/` plus the `postgres-integration` CI
  job that exercises them. The Risk Register row on row-inference
  error quality now references the six-case `compile-fail` suite
  and `compile-fail/FINDINGS.md`, with the two noisy cases (03
  payload-type mismatch and 04 missing-tag) flagged as the v0.2
  `Fail`-polish backlog.
- `rio-config-file`: 12 additional edge-case tests for the
  pure `parseDotenv` / `flattenJson` helpers. `parseDotenv`
  picks up unterminated-single-quote rejection, the full
  `\t` / `\r` / `\\` / `\"` escape set in double-quoted
  values, `=` inside the value, empty bare and empty quoted
  values, `#` literal inside single quotes, `#` adjacent to
  non-whitespace not being treated as a comment, and
  whitespace-around-key trimming. `flattenJson` picks up
  nested-null-with-sibling, three-level nesting, deeply
  nested array rejection with the full path preserved, and
  a top-level empty object yielding an empty map. `npx
  spago test -p rio-config-file` now reports `30/30 tests
  passed` (up from 18).
- Top-level `README.md` and `examples/todo-api/README.md`:
  the todo-api blurb said "in-memory persistence" (stale; the
  example has used real Postgres since the rio-postgres
  driver wrapper landed) and the per-example README still
  described migrations as "an idempotent create table if not
  exists" (also stale; the example goes through
  `RIO.Postgres.Migrate.migrate` with a recorded version,
  an advisory lock, and the `__rio_migrations` bookkeeping
  table). Both descriptions updated to match the actual
  shape.
- `rio-postgres/README.md`: refreshed the Testing section. The
  previous text claimed CI did not run the integration suite
  yet, which is stale: the `postgres-integration` job exercises
  the suite against a service container, plus the
  `rio-postgres-json`, `rio-postgres-migrate`, and
  `rio-example-notify` companion runs. The new section spells
  out the local docker-compose path and the
  `PG_CONNECTION_STRING` skip behaviour for contributors
  without Postgres.
- `compile-fail`: three additional negative-build cases bring the
  driver from 3 to 6 passing cases. Case 04 covers `catchTag` for
  a tag that isn't in the error row; case 05 covers `mapError`
  whose residual error row is then handed to `runRIO'`; case 06
  covers `provideAll` with a record that's missing a required
  field. `compile-fail/FINDINGS.md` records the compiler output
  for each case and the readability verdict, and the "patterns
  not yet captured" section now correctly notes that the
  "provide twice" trap is not actually a compile-fail target
  (row polymorphism happily extends with a fresh-row tail).
- `docs/03-errors.md`: added the missing `## Pointers` section
  (every other 03-15 reference doc has one). The new section
  links to `src/RIO/Error.purs`,
  `test/Test/RIO/ErrorSpec.purs`, `docs/14-causes.md` (for the
  defect channel), and `examples/worker-pool/` (which raises a
  `jobFailed :: String` typed failure, retries on it, and
  drives a `parTraverseCause` pre-flight pass).
- Worked-example cross-links added to three more numbered
  reference docs: `docs/05-resources.md` now points at
  `examples/notify/` and `examples/todo-api/` for resource-safe
  service shutdown; `docs/06-concurrency.md` points at
  `examples/worker-pool/` for the fan-out / `parTraverseCause` /
  `forkScoped` / `Deferred` mix; and `docs/08-scheduling.md`
  points at `examples/worker-pool/` for the
  `intersect (recurs n) (exponential base 2.0)` retry shape.
- CI now exercises every adapter test suite and every example
  runtime. The build matrix runs `npx spago test` for
  `rio-config-file`, `rio-http`, and `rio-otel`; builds
  `rio-postgres-json` and `rio-postgres-migrate`; and runs
  `worker-pool`, `stream-pipeline`, `sink-analytics`, and
  `config-loader` as smoke checks. The postgres integration job
  also drives `rio-postgres-json` and `rio-postgres-migrate`
  against the service container, not just `rio-postgres`. The
  format-check step was updated to include `rio-config-file`,
  `rio-postgres-json`, and `rio-postgres-migrate`, matching the
  refreshed `npm run format:check` scripts.
- `rio-otel`: test suite for `RIO.Tracer.OTel`. Pins the
  adapter's own bookkeeping (sequential `SpanId` allocation
  starting at 1, `currentSpan` reporting the latest open span
  as parent, stack pop on `endSpan` restoring the parent,
  `endSpan` of an unknown or already-closed span as a no-op,
  `endSpan` of a non-current span filtering it out of the
  stack, `addAttribute` safety against unknown ids, and
  per-tracer counter isolation). Runs against the no-op tracer
  that `@opentelemetry/api` returns when no SDK is registered;
  the end-to-end SDK round-trip lives in `examples/otel-demo/`.
  `npx spago test -p rio-otel` now reports `12/12 tests
  passed`.
- `rio-http`: test suite for `RIO.HTTPurple.Auth`,
  `RIO.HTTPurple.Request`, and `RIO.HTTPurple.Middleware`.
  Covers `bearerAuthConfig` prefix shape, `requireAuth`
  success / missing-header / mismatched-header / case-
  insensitive-header / scheme-required paths,
  `defaultRequestIdHeader`, `mkRequestContext`'s header-
  honouring, monotonic-fallback, custom-header-name, and
  counter-isolation behaviours, and `withRequestContext`'s
  log-line pair, `request.id` / `request.method` /
  `request.path` annotation stamping, `requestId` `Local`
  set/restore, and `duration_ms` annotation on completion
  (driven by `RIO.Test.Logger` and `RIO.Test.Clock`). Fills
  the previously empty test stanza on the package; `npx spago
  test -p rio-http` now reports `20/20 tests passed`.
- Numbered reference docs filled in for every substantive module:
  `docs/04-layers.md` (`Layer` construction, composition,
  `provideLayer`, resource-safe layers), `docs/05-resources.md`
  (`acquireRelease`, `ensuring`, `Scope` / `scoped`,
  `RIO.Resource.Do`), `docs/14-causes.md` (the `Cause` algebra
  and cause-collecting combinators), and `docs/15-config.md`
  (the `Config` DSL, sources, `Secret`, `Rotating`). The
  numbered doc set now runs 01-15 contiguously and covers every
  module under `src/RIO/`. The `forthcoming` pointers in
  `docs/07-testing.md` and both migration guides are resolved.
- `docs/sink-design.md` rewritten as design notes (why the
  shape, why single-fiber `zipPar`, why no Channel) now that
  `RIO.Sink` has shipped.
- `examples/config-loader/`: loads typed configuration from a
  sample `.env` file via `rio-config-file`. Exercises
  `dotenvFileSource`, the `Config` DSL (with `withDefault` and
  `secret`), `Secret` redaction at print time, and the
  failure path through `prettyConfigError`. First example
  that uses `rio-config-file` end-to-end.
- `examples/sink-analytics/`: a single-pass analytics demo over
  a synthetic HTTP request log. Composes five small sinks
  (`count`, `filterIn isError count`, `mapInput _.latencyMs`
  over a max-fold, a path-set fold, and `find` for the first
  slow request) with `zipPar` and runs the result against
  `fromArray`. One stream pass produces the full summary.
- `RIO.Sink`: first-class composable terminating consumers for
  `RIO.Stream`. `Sink r e i a` consumes some prefix of `i`s and
  produces an `a`. The shape is `Need k finish | Halt a` so
  short-circuiting sinks (`take`, `find`, `any`, `all`) finalise
  cleanly against infinite streams. Ships `drain`, `head`,
  `last`, `count`, `collect`, `foldL`, `foldM`, `take`, `find`,
  `any`, `all`, `mapResult`, `mapInput`, `filterIn`, `andThen`,
  `zipPar`, `zipParWith`, and `runSink`. `zipPar` runs two
  sinks in lockstep against the same stream on one fiber;
  early-halt of one side is remembered while the other
  continues. See `docs/13-streams.md` and `docs/sink-design.md`.
- `RIO.Config.Rotating`: a refreshable cell for values that
  can change at runtime (typically a rotating `Secret`).
  `newRotating` allocates a cell with an initial value;
  `readRotating` / `writeRotating` are atomic read / write
  primitives. `withRotation` bundles a loader: it runs the
  loader once to populate the cell and returns a `refresh`
  action that re-runs the loader and overwrites the cell. The
  module imposes no rotation policy; polling, signal handling,
  or other triggers are left to the caller.
- `rio-config-file`: new adapter package providing
  `dotenvFileSource` and `jsonFileSource`. Both read a file
  from disk and return a `RIO.Config.Source` ready to feed
  into `load`. `parseDotenv` and `flattenJson` are exposed as
  pure helpers for tests and in-memory callers. The JSON
  flattener joins nested object keys with `_`, matching the
  way `RIO.Config.nested` qualifies keys, so the same
  `Config` descriptor works against env, dotenv, and JSON
  sources without modification.
- `RIO.Stream.Par`: parallel stream combinators. `mergeAll`
  fans in N producer streams onto a shared bounded queue (one
  fiber per producer; first observed failure shuts the queue
  down and is propagated on the consumer's next pull). `merge`
  is the two-stream convenience. `mergeMap` materialises the
  outer stream then merges every inner stream concurrently.
  `broadcast` fans one upstream out to N consumer streams over
  per-consumer bounded queues with end-to-end backpressure.
  `partition` routes each upstream element to one of N buckets
  via a key function (mod N, normalised for negative keys).
  All four share the same failure model: first failure wins;
  sibling producers exit naturally when they find the queue
  closed.
- `RIO.Stream.Resource`: `bracketStream` is a single-element
  resource-acquiring stream whose release is registered with
  the enclosing `scoped` block. Compose with `flatMap` to
  thread the acquired resource through a multi-element
  downstream. Release runs on every termination path; if
  acquire fails, no finalizer is registered.
- `RIO.Stream` now exports `Stream(..)` and `unStream` so
  companion modules (e.g. `RIO.Stream.Par`,
  `RIO.Stream.Resource`) can step the underlying `RIO` when
  building new combinators. End-user code should still reach
  for the combinator surface; the constructor is exposed for
  library extension only.

- `RIO.Resource.Do`: qualified-do sugar over
  `RIO.Resource.acquireRelease`. Each `<-` inside a
  `Resource.do` block desugars to an `acquireRelease`, with the
  block's continuation becoming the `use` callback. Flattens
  what would otherwise be nested brackets when one computation
  opens several resources. Release ordering matches
  `acquireRelease`: LIFO on every termination path (success,
  typed failure, defect, kill). Plain `RIO` statements
  interleaved between acquisitions wrap with
  `Resource.liftRIO`. Verified against hand-nested
  `acquireRelease` (identical event ordering) and against
  typed-failure / failed-acquire paths in `Test.RIO.Resource.Do`.
- `RIO.Concurrency.Par`: qualified-`ado` sugar for running
  independent `RIO` actions concurrently and combining their
  results under `Control.Parallel`'s `ParAff`. Use with
  `Par.ado`, not `Par.do` (qualified-do for parallel
  composition would still sequence). Failure bias: leftmost
  typed failure wins, but every branch is allowed to run to
  completion. For short-circuiting fan-out (cancel the loser on
  first failure), keep using `RIO.Concurrency.parPair` /
  `parTuple`. Verified at 3x speedup vs sequential `do` in
  `Test.RIO.Concurrency.Par`.

- `rio-http` workspace package: extracts the reusable HTTP
  pieces of the todo-api example into a standalone adapter so
  apps that pair `rio` with [HTTPurple](https://pursuit.purescript.org/packages/purescript-httpurple)
  can pick them up without copying.
  - `RIO.HTTPurple.Request` exposes a `RequestContext` record
    (method, path, requestId, headers) plus `mkRequestContext`,
    `newRequestCounter`, and `defaultRequestIdHeader` for
    snapshotting an HTTPurple `Request` into a flat shape free of
    the route type variable. Honours an inbound
    `X-Request-Id` header when present; falls back to a
    monotonic `req-N` allocated from a per-process counter.
  - `RIO.HTTPurple.Middleware.withRequestContext` wraps any
    `RIO` action so every emitted log line carries
    `request.id` / `request.method` / `request.path`, writes
    the request id into a `Local String` for downstream
    correlation, and emits a `request received` /
    `request completed` (or `request failed`) pair around the
    body with elapsed milliseconds and a success / failure
    verdict. The wrapped action's error row is preserved.
  - `RIO.HTTPurple.Auth.requireAuth` is a polymorphic typed
    failure: takes a `Proxy sym` and a payload supplied by the
    caller so each consuming app can choose its own tag
    (`unauthorized`, `forbidden`, ...) and payload on its
    own error row. `bearerAuthConfig` builds a config whose
    `expected` field is `"Bearer " <> token`.
- CI builds `rio-http` on every PR and the
  `purs-tidy` format check now covers the `rio-http/` source tree.

- `rio-postgres` workspace package: an adapter wrapping
  [`purescript-postgresql`](https://pursuit.purescript.org/packages/purescript-postgresql)
  (the `node-postgres` / `pg` driver) so apps can talk to
  Postgres through a row-typed `Postgres` service. Each
  combinator surfaces driver failures on a caller-chosen typed
  tag carrying `PgError` (a thin newtype around the library's
  `NonEmptyArray Error`), keeping the row layout up to the
  consuming app.
  - `RIO.Postgres` exposes the `Postgres` service token,
    `PgError` plus `pgErrorMessage`, and a small set of smart
    constructors: `withClient` brackets a client from the pool
    for a callback (release on every termination path),
    `query` / `exec` run a one-shot statement on a fresh
    client, and their `*Using` variants thread an existing
    client for in-transaction chaining. `withTransaction`
    wraps a block in `BEGIN` / `COMMIT`, rolling back on any
    typed failure on the chosen tag and re-raising it.
    Re-exports `Pool`, `Client`, `AsQuery`, `FromRows`, and
    the underlying `Error` shape so consumers don't need to
    depend on the driver package directly.
  - `RIO.Postgres.Layer.postgresLayer` builds a fresh pool
    from a `node-postgres` config record and registers the
    pool's `Pool.end` shutdown as a finalizer on the
    surrounding scope, so pool drain is guaranteed on every
    exit path of the scope the layer is built into.
- CI builds `rio-postgres` on every PR and the `purs-tidy`
  format check covers the `rio-postgres/` source tree.

### Changed

- `examples/todo-api/Middleware.purs` is now a thin app-shim
  over `rio-http`. It re-exports `RequestContext` /
  `AuthConfig` / `withRequestContext` verbatim and pre-applies
  `requireAuth` against the example's `unauthorized` typed
  failure so call sites stay unchanged.

### Research

- `spikes/qualified-do/`: explores PureScript's qualified-do as
  ergonomic sugar over `RIO` patterns. Two candidates earn their
  keep: `Resource.do` flattens nested `acquireRelease` blocks
  (verified to produce identical LIFO release ordering against
  the hand-nested form), and `Par.ado` runs each `<-` line
  concurrently under `Control.Parallel` (clocked at 102ms vs
  302ms for three 100ms branches). The findings call out one
  ergonomic gotcha (plain `RIO` lines inside `Resource.do` need
  an explicit `liftRIO`) and the things qualified-do
  fundamentally cannot do (type-directed implicit `Proxy` at the
  `<-` site, implicit `atomically` lifts that mix `STM` and
  `RIO`, generator-style `yield` / `await`). See
  `spikes/qualified-do/FINDINGS.md`. CI builds and runs the
  spike on every PR. The two winning candidates are scoped for
  inclusion in the main package under
  `RIO.Resource.Do` / `RIO.Concurrency.Par` once the surface is
  reviewed.

### Changed

- `examples/todo-api/`: migrated to `RIO.Logger` for structured
  logging and demonstrates two `RIO`-native middleware
  combinators built on the new logging / local services. A new
  `Example.TodoApi.Middleware` module ships
  `withRequestContext`, which opens a per-request
  `withFields` block stamping `request.id` /
  `request.method` / `request.path` on every emitted line
  (including domain log lines), writes the request id into a
  `Local String` so downstream code can correlate without
  threading an argument, and emits a `request received` /
  `request completed` (or `request failed`) pair around the
  body with elapsed milliseconds and a success / failure
  verdict; `requireAuth` is a bearer-token check that raises
  the `unauthorized` typed failure on the existing error row.
  The handlers stay domain-focused: logging correlation,
  per-request id propagation, and auth are layered on by the
  middleware in `Main.purs`, not threaded through every call.
  Inbound `X-Request-Id` headers are honoured for trace
  correlation; otherwise the server assigns a monotonic
  `req-N`. The example's `ApiError` row picks up an
  `unauthorized :: Unit` tag handled in `renderApiError`
  alongside the existing `notFound` case. The walkthrough in
  `examples/todo-api/README.md` is updated with the new
  smoke-test curls (auth path, `X-Request-Id`, JSON 400,
  method 405) and a sample of the resulting structured log
  output.

### Added

- `spikes/phase-9-review/`: randomised stress harness covering
  the recently-added modules. Eight scenarios, one invariant
  each: `RIO.Logger` nests `withFields` up to eight levels
  deep under random typed failures and fork/join, then asserts
  the annotation set is empty after the program returns;
  `RIO.Local` does the same shape on a `Local Int` with an
  added kill path that interrupts a forked child mid-flight;
  `RIO.STM.TQueue` runs up to four producers in parallel
  against up to four forked consumers and asserts count and
  sum match across the queue; `RIO.STM.THub` covers all four
  back-pressure strategies (Unbounded fan-out with multiple
  subscribers; Bounded back-pressure with a forked publisher
  forced to retry while a single consumer drains; Sliding
  drop-oldest with a non-draining subscriber checking the last
  `buffer` values survive in order; Dropping drop-new with a
  non-draining subscriber checking the first `buffer` values
  survive and overflowing publishes return `false`);
  `RIO.STM.TSemaphore` exercises `withTSemaphore` with random
  typed failures and mid-hold fiber kills and asserts every
  permit is returned. 250 iterations per scenario per
  invocation (2000 total). Across four consecutive local runs
  (8000 total iterations) the harness reports zero invariant
  violations. See `spikes/phase-9-review/FINDINGS.md`. CI
  builds and runs the spike on every PR.
- `RIO.STM.THub` module: transactional publish/subscribe hub.
  Each published value fans out to every active subscriber's
  private buffer; subscribers consume independently. Four
  back-pressure strategies chosen at construction time:
  `newBoundedTHub n` (producer retries while any subscriber is
  full), `newSlidingTHub n` (drops oldest on full, never
  blocks), `newDroppingTHub n` (drops new on full, never
  blocks, returns `false` when any subscriber dropped),
  `newUnboundedTHub` (never blocks, never drops, susceptible to
  memory growth on slow consumers). Subscribers register with
  `subscribeTHub` and consume with `takeSubscription` /
  `tryTakeSubscription`; `unsubscribeTHub` removes a
  subscription and drops its buffered values. Prefer
  `withSubscription` for the common case: it brackets
  subscribe/unsubscribe against an `RIO` action so the
  subscription is released on every termination path. A new
  subscriber sees only values published after it registers.
  See `docs/09-stm.md`.
- `RIO.Logger` module: structured logging service. `Logger` is a
  record of `log` / `getAnnotations` / `setAnnotations`
  operations carried in the environment row at the `logger`
  field. Five levels: `LogTrace`, `LogDebug`, `LogInfo`,
  `LogWarn`, `LogError` (no `LogFatal`; unrecoverable failures
  belong on the defect channel). Smart constructors `logTrace`
  / `logDebug` / `logInfo` / `logWarn` / `logError` emit at
  each level. `withField key value action` and `withFields
  fields action` scope a batch of `(key, value)` annotations to
  a block; the previous annotation set is restored by
  `Aff.finally` on every termination path. Annotation merging
  shadows existing keys with their inner replacements and
  preserves attach order so backends can render fields in input
  order. Backends shipped: `noopLogger` (discards emissions,
  retains annotation scoping), `consoleLogger` (writes
  `[LEVEL] message  k1=v1, k2=v2` lines to
  `Effect.Console.log`), and `RIO.Test.Logger.newRecordingLogger`
  (in-memory recorder for tests). Annotations are stored in a
  shared `Ref`; see `docs/12-logging.md` for the documented
  fork-inheritance trade-off (the same one `RIO.Tracer` and
  `RIO.Local` make).
- `RIO.Local` module: ambient state with scoped overrides.
  `Local a` is a typed cell created by `newLocal` (or
  `newLocalEffect` for callers building their environment
  outside an `RIO` action) with `get` / `set` / `update`
  operations and a `locally fl value action` combinator that
  scopes a value to a block. The restore is guaranteed by
  `Aff.finally` on every termination path (success, typed
  failure, defect, interrupt). Backed by `Effect.Ref`, so a
  forked child fiber observes the parent's current value and a
  child's writes are visible to the parent; this is the same
  implicit-context model `RIO.Tracer` uses. See
  `docs/11-fiber-local.md` for use cases, the comparison to
  ZIO `FiberRef`, and the documented fork-inheritance
  trade-off.

### Added

- `RIO.Concurrency.timeout :: Milliseconds -> RIO r e a -> RIO r e
  (Maybe a)`. Race an action against a deadline; on timeout the
  action is interrupted and `Nothing` is returned. Typed failures
  from the action propagate unchanged.
- `RIO.Concurrency.parTraverseN :: Int -> (a -> RIO r e b) -> Array
  a -> RIO r e (Array b)`. Bounded-concurrency traversal that
  chunks the input array into `n`-sized groups and `parTraverse`s
  each chunk in turn.
- `RIO.Concurrency.uninterruptible :: RIO r e a -> RIO r e a`. Wrap
  a critical section so kills are queued until the inner action
  completes. Sits on top of `Aff.invincible`.
- `RIO.Concurrency.forkScoped :: Scope -> RIO r e a -> RIO r e'
  (Fiber e a)`. Fork into a scope: when the scope exits the fiber
  is interrupted as part of its LIFO finalizer pass. The
  structured-concurrency counterpart of `fork`.
- `RIO.Resource.ensuring :: RIO r e a -> RIO r () Unit -> RIO r e
  a`. `finally`-style finalizer guarantor without the
  acquire/release split of `acquireRelease`.
- `RIO.Deferred` module: one-shot write-once cell over
  `Effect.Aff.AVar` for fiber handshakes. `makeDeferred`,
  `succeedDeferred`, `failDeferred`, `awaitDeferred`,
  `pollDeferred`.
- `RIO.Layer.passthrough :: Union rOut rIn rPassed => Layer rIn e
  rOut -> Layer rIn e rPassed`. Extend a layer's output row with
  the labels it required as input, so downstream consumers see
  both. Closes DX-1.
- `RIO.Schedule` module: pure scheduling policies for retry and
  repeat. `Schedule r i o` with `recurs`, `spaced`, `exponential`,
  `forever`, `once`; combinators `andThen`, `intersect`,
  `whileInput`, `jittered`, `mapSchedule`; runners `repeat`,
  `retry`, `retryOrElse` that sleep via the `Clock` service so a
  virtual-time test clock can drive scheduled programs
  deterministically. `step` exposes one decision for tests that
  sample a schedule's delay distribution. The error row is fixed
  to `()`; schedules cannot themselves fail with a typed error.
  See `docs/08-scheduling.md`.
- `Benchmarks.Gate`: developer-runnable performance regression
  gate. Runs the same scenarios as `Benchmarks.Main`, compares
  each one's mean wall-clock per iteration against a baseline
  picked by profile (`RIO_GATE_PROFILE` env var, default
  `local-m1-pro`; `ci-ubuntu-latest` is the in-repo CI profile),
  prints a one-row table per scenario plus a single-line
  `BASELINE_JSON` blob of observed means for capture, and exits
  non-zero if any scenario's mean is more than 3x its baseline.
  Threshold is deliberately generous to tolerate
  machine-to-machine variance. Scenarios with no baseline in the
  active profile are reported as `n/a` and do not contribute to
  the regression count. The CI workflow now runs the gate on the
  `node 20` matrix leg in informational mode
  (`continue-on-error: true`) so the `BASELINE_JSON` line can be
  mined to populate the `ci-ubuntu-latest` profile; the gate
  becomes required by flipping that one flag once the baseline
  has been captured. See `docs/performance.md` for the full
  capture procedure.
- `random` to the main `rio` package's dependency manifest
  (used by `RIO.Schedule.jittered`; was previously available
  transitively through the test stack only).
- `RIO.STM` module: software-transactional memory. `TRef a`,
  `STM e a` (with `Functor` / `Apply` / `Bind` / `Monad`
  instances), `newTRef`, `readTRef`, `writeTRef`, `modifyTRef`,
  `retry`, `check`, `orElse`, `failSTM`, and `atomically`. The
  implementation uses the JS event loop's lack of preemption
  directly: an `STM` body is a synchronous `Effect` whose
  intermediate writes are unobservable to other fibers, so commit
  needs neither version checks nor pessimistic locks. `retry`
  suspends until any read `TRef` is written, via waiter callbacks
  fired from the writer's commit phase. Typed failures abort the
  transaction (no writes apply) and surface on the parent's row.
  See `docs/09-stm.md`.
- `RIO.STM.TQueue`: unbounded FIFO queue built on a single
  `TRef (Array a)`. Surface: `newTQueue`, `writeTQueue`,
  `readTQueue` (retries when empty), `tryReadTQueue`,
  `peekTQueue`, `isEmptyTQueue`, `lengthTQueue`. Underlying
  enqueue/dequeue are `Array.snoc` / `Array.uncons` (O(n) on the
  JS backend); the API leaves room for a deque-based replacement.
- `RIO.STM.TMap`: transactional map keyed by an `Ord` type,
  backed by a single `TRef (Map k v)`. Surface: `newTMap`,
  `insertTMap`, `lookupTMap`, `deleteTMap`, `memberTMap`,
  `sizeTMap`, and `awaitKey` (retries until a key is present).
  Wakeups are not key-indexed; any write to the underlying TRef
  re-checks the predicate, which suits "wait on handler
  registration" patterns and is fine for low-churn maps.
- `RIO.STM.TSemaphore`: counting semaphore on a single
  `TRef Int`. Surface: `newTSemaphore`, `acquireTSemaphore`,
  `acquireN`, `releaseTSemaphore`, `releaseN`,
  `availableTSemaphore`, and `withTSemaphore`, which brackets
  an acquire/release pair around an `RIO` action via
  `acquireRelease` so the permit is returned on every
  termination path.
- `ordered-collections` to the main `rio` package's dependency
  manifest (used by `RIO.STM.TMap`).
- `RIO.Tracer` module: tracing service with named spans, status
  recording (`SpanOk` / `SpanFailed` / `SpanInterrupted`), and
  string attributes. `withSpan` brackets an action: opens a span
  as a child of the currently-active span, runs the action,
  closes the span with the appropriate status on every
  termination path (success, typed failure, fiber kill).
  `addAttribute` attaches a key/value pair to the currently
  active span; `currentSpan` reports it. `noopTracer` discards
  every operation. Parent context is implicit and survives the
  common "fork inside a span" case in the JS single-event-loop
  model; see `docs/10-tracing.md` for the explicit caveats.
- `RIO.Test.Tracer` module: `newRecordingTracer` returns a
  `Tracer` plus a `snapshot` action that returns the recorded
  spans in start order. Virtual time advances by 1 per
  `startSpan` / `endSpan`, making span ordering deterministic in
  tests.
- `RIO.Metrics` module: counter / gauge / histogram service
  shape with `recordCounter`, `recordGauge`, `recordHistogram`
  and call-site-readable aliases (`incrementCounter`,
  `setGauge`, `observeHistogram`). `noopMetrics` discards every
  emission.
- `RIO.Test.Metrics` module: `newRecordingMetrics` returns the
  service plus a `snapshot` action returning every emission
  with its kind (`Counter` / `Gauge` / `Histogram`), name, and
  value.
- `rio-otel` package (`rio-otel/`): OpenTelemetry adapter for
  `RIO.Tracer`. `RIO.Tracer.OTel.makeOTelTracer name` returns a
  `Tracer` record that forwards every span lifecycle, attribute
  write, and parent / child relationship to an
  `@opentelemetry/api` tracer; call sites that use `withSpan`,
  `addAttribute`, or `currentSpan` keep working verbatim. Status
  maps `SpanOk -> OK`, `SpanFailed -> ERROR`,
  `SpanInterrupted -> ERROR` with message `"interrupted"`. With
  no OTel SDK registered the adapter is silent (the global API
  returns a no-op tracer). An end-to-end demo wiring the adapter
  to `BasicTracerProvider` + `InMemorySpanExporter` lives at
  `examples/otel-demo/`.

### Changed

- `parTraverse` and `parSequence` now short-circuit on the first
  typed failure, cancelling sibling fibers. The earlier
  implementation ran every branch to completion before surfacing
  the first `Left`. The new
  behaviour matches ZIO `foreachPar` / Effect-TS `forEach` with
  `concurrency: "unbounded"` and is implemented by throwing a
  sentinel defect from the failing branch (caught by `Aff.attempt`
  at the boundary) plus a shared `Ref` for the first-failure value.
  Successful programs are unaffected; programs that depended on
  the old "run to completion" semantics will see siblings
  interrupted instead of completing.
- `zipPar` short-circuits similarly: the first `Left` from either
  branch cancels the other.
- `raceAll` is now implemented in terms of
  `Control.Parallel.parOneOfMap` rather than a left-fold of
  pairwise `race`. The behaviour is the same (first to complete
  wins; losers are interrupted), but every branch is started in
  parallel rather than racing pairwise.

## Earlier work (build-plan phases 0–8)

Nothing below has been released. These entries describe the
phase-by-phase work that landed on `main` against the original
build plan, covering the production core: typed environment row,
typed error row, resource-safe bracket and scope primitives,
layer composition, structural concurrency with cancellation,
virtual-time testing, and adapters for `purescript-spec`. See
`docs/` for the walkthroughs, and `docs/migrating-from-zio.md` /
`docs/migrating-from-effect-ts.md` for idiom-by-idiom mappings.

### Release-prep work (not actually released)

- Version string set to `0.1.0` in `spago.yaml` as a placeholder.
- README rewritten as a landing page: 30-second tour, install
  note, module-by-module surface, links to walkthrough docs and
  the worked example, build and run instructions.

### Added

- Initial project scaffold (Phase 0.1).
- `RIO.Internal` module defining the `RIO r e a` newtype, with the
  data constructor available for in-library use only (Phase 1.1).
- `RIO.Core` module exposing `RIO` as an opaque type plus `runRIO`,
  `runRIO'`, and `unsafeRunRIO` (Phase 1.1).
- `Functor`, `Apply`, `Applicative`, `Bind`, and `Monad` instances
  for `RIO r e`, with sampled law checks in the test suite (Phase 1.2).
- `MonadEffect` and `MonadAff` instances for `RIO r e` (Phase 1.3).
- `RIO.Error` module with `fail` for raising typed failures, re-exported
  from `RIO.Core` (Phase 1.3).
- `docs/01-core-type.md`: walkthrough of the three type parameters and a
  comparison with ZIO and Effect-TS (Phase 1.4).
- `RIO.Env` module with `ask` and `asks` for reading services out of the
  environment row (Phase 2.1).
- `provide` in `RIO.Env`: single-service injection that shrinks the
  required row by one field. The `Lacks` constraint from the original
  draft is dropped, per the Phase 0.4 spike's LE-1 finding; the internal
  insertion uses `Record.Unsafe.unsafeSet`, which is safe under the
  `Cons` relation (Phase 2.2).
- `provideAll` in `RIO.Env`: full-environment injection that produces a
  `RIO () e a` runnable directly via `runRIO` or `runRIO'` (Phase 2.3).
- `examples/logger/`: a complete `Logger` service plus a runnable
  example demonstrating the idiomatic service shape (record of
  `Aff`-valued operations + smart constructors lifting into `RIO`)
  (Phase 2.4).
- `docs/02-services.md`: the service convention, including two traps to
  avoid (polymorphic operation fields, and using `asks` to project an
  operation function) (Phase 2.4).
- Row-inference regression test asserting that a do-block with two
  disjoint `ask`s infers a row covering both services with the
  environment-row variable kept open (Phase 2.5).
- `RIO.Test` module with `mockService` (a more readable alias for
  `provide`) and `recording` (a small helper for capturing service-call
  histories into a `Ref` for test assertions) (Phase 2.6).
- `compile-fail/` test driver and the first negative case: providing a
  service whose value type doesn't match the required service. CI now
  runs the driver alongside the regular test suite.
- `spikes/phase-2-review/`: Phase 2 review cycle. Ten realistic service
  compositions written against the production `RIO.Core` API with no
  user-supplied type signatures; `FINDINGS.md` reproduces each inferred
  type verbatim. Confirms LE-1 (the `Lacks` leak from the Phase 0.4
  spike) is gone in the production API and surfaces no new regressions.
  CI builds the spike on every PR.
- `catchTag` in `RIO.Error`: catch one named failure tag and remove it
  from the error row, with the handler free to introduce new tags
  (Phase 3.1).
- `catchAll` and `mapError` in `RIO.Error`: replace the error row in
  bulk via an effectful handler or a pure translation respectively;
  `rethrow` as the identity handler for selective passthrough inside
  `catchAll` (Phase 3.2).
- `die`, `sandbox`, `unsandbox` in `RIO.Error`: distinguish typed
  failures (in the row) from defects (`Aff` exceptions); `sandbox`
  reifies defects into the success channel as `Either Error a`
  without absorbing typed failures (Phase 3.3).
- `docs/03-errors.md`: walked-through example narrowing a three-tag
  error row down to `()`, with the compiler's actual inferred type
  quoted at each step from
  `spikes/phase-2-review/src/Spike/ErrorsDocFixture.purs` (Phase 3.4).
- Phase 3 review cycle: two new compile-fail cases (`runRIO'` with a
  leftover error tag; `catchTag` with a wrong payload type) plus
  `compile-fail/FINDINGS.md` rating the readability of each compiler
  message and listing candidates for custom `Fail` instances.
- `RIO.Resource` module with `acquireRelease`: bracket-style primitive
  that guarantees the release action runs on every termination path of
  the use phase (success, typed failure, defect, or external fiber
  kill). The release path has an empty error row by construction; if
  acquisition itself fails, release is not invoked. Builds directly on
  `Effect.Aff.bracket`, whose release phase is uninterruptible by
  default (Phase 0.5 spike, scenario S6) (Phase 4.1).
- `Scope`, `addFinalizer`, and `scoped` in `RIO.Resource`: introduce a
  scope under the `scope` service label, push `Aff` finalizers onto its
  stack, and run them LIFO on exit on every termination path. A
  finalizer that throws does not stop subsequent finalizers from
  running; exceptions are swallowed for now so a single leak cannot
  cascade. Aggregating finalizer errors is deferred to a later phase
  (Phase 4.2).
- Phase 4 review cycle: `spikes/phase-4-review/` opens 1000 nested
  scopes per iteration, picks a random depth and termination mode
  (success, typed failure, defect), and asserts the resulting event
  log shows every `register-k` matched by a `finalize-k` in LIFO
  order. A second scenario forks the program and injects a random
  `killFiber` during an innermost `Aff.delay` and applies the same
  check. 100 iterations per invocation, replayed in CI. Across four
  consecutive local runs (400 total iterations) the harness reports
  zero leaks and zero LIFO violations. Findings live in
  `spikes/phase-4-review/FINDINGS.md`.
- `RIO.Layer` module with the `Layer rIn e rOut` newtype, `fromRecord`
  (lift a fixed record), `fromRIO` (build a record from an `RIO` that
  can `ask` for inputs, lift `Aff`, and register finalizers via the
  `scope` service), and `buildLayer` (a closing runner intended for
  test layers that do not own resources) (Phase 5.1).
- `andThen` and `combine` in `RIO.Layer`, with operator aliases
  `(>>>)` (`infixr 1`) for sequential composition and `(<+>)`
  (`infixr 7`) for horizontal composition. `(>>>)` shadows
  `Control.Semigroupoid.(>>>)`; `RIO.Core` re-exports only the named
  forms so `import Prelude` keeps the standard `(>>>)` accessible.
  `combine` requires `Prim.Row.Union` on the output rows; output rows
  with overlapping labels are rejected by the compiler (Phase 5.2).
- `provideLayer` in `RIO.Layer`: build a layer and run an inner
  program in the layer's services, unioning layer and program error
  rows via `Prim.Row.Union e e' eOut`. A single scope spans both the
  layer build and the program run, so finalizers registered by the
  layer release after the program completes on every termination
  path: success, typed failure, and defect (Phases 5.3 and 5.4). The
  forward error-row expansion uses `Data.Variant.expand` against the
  supplied `Union`; the program-side expansion uses `unsafeCoerce`
  because PureScript's row solver can't recover the symmetric
  `Union e' e eOut` instance from the user-supplied one. The cast is
  safe at runtime: `expand` itself is `unsafeCoerce`, and the
  constraint already proves every label of `e'` is in `eOut`.
- `Scope` constructor exported from `RIO.Resource` for in-library
  use by `RIO.Layer.provideLayer`. `RIO.Core` continues to re-export
  only the opaque type, so the public surface is unchanged.
- `spikes/phase-5-review/`: Phase 5 review cycle. A six-service
  layered application (`Config`, `Logger`, `Clock`, `Cache`,
  `Database`, `UserService`) split across three layers, including
  a failing layer (`dbConnect` when `databaseUrl` is empty) and a
  resourceful layer (registers `cache-flush` and `db-close`
  finalizers). Three scenarios assert exact event sequences:
  happy path, layer-level failure, and program-level typed failure
  after service use. All three pass. `FINDINGS.md` records one DX
  issue worth tracking: the lack of a passthrough operator for
  sequential composition (DX-1, candidate for a later phase). CI
  builds and runs the spike on every PR.
- `RIO.Concurrency` module with `Fiber e a`, `fork`, `join`, and
  `interrupt` (Phase 6.1). `fork` and `interrupt` are infallible
  from the caller's perspective and leave the caller's error row
  free (instead of pinning it to `()`) so they compose cleanly
  inside a do-block whose surrounding row is non-empty: this is the
  one departure from the build plan's literal signature, made
  because the `()` form forces the entire surrounding do-block to
  have `()` for its error row. `Fiber e a` wraps an
  `Effect.Aff.Fiber (Either (Variant e) a)`; typed failures from
  inside a fiber surface on `join` as `Left v` on the joiner's
  row, defects (including the kill exception from `interrupt`)
  propagate as `Aff` exceptions and are observable via
  `RIO.Error.sandbox`. The cancellation guarantees come from the
  Phase 0.5 spike scenarios S1 / S3 / S4.
- `parTraverse`, `parSequence`, and `zipPar` in `RIO.Concurrency`
  (Phase 6.2). Layered on `Effect.Aff`'s `ParAff` applicative via
  `Control.Parallel.parTraverse` and `Effect.Aff.parallel /
  sequential`. Failure semantics: all branches run to completion
  and the first `Left` (in array order, or favouring the left side
  for `zipPar`) is surfaced; first-failure racing semantics are
  reserved for `race` in Phase 6.3. Timing tests confirm two 100ms
  actions complete in ~100ms rather than ~200ms. `parallel`,
  `arrays`, `datetime`, `integers`, `newtype`, and `now` added to
  the package's dependency manifest.
- `RIO.Clock` module with the `Clock` service (`now :: Aff
  Milliseconds`, `sleep :: Milliseconds -> Aff Unit`), smart
  constructors `now` and `sleep` that read the service from the
  environment row, and `liveClock` backed by `Effect.Now` and
  `Effect.Aff.delay`. The service operations are `Aff`-valued
  following the `docs/02-services.md` convention; the
  cancellation guarantees of `liveClock.sleep` come from
  `Aff.delay` and match the Phase 0.5 spike's S1 scenario
  (Phase 7.1). `datetime` and `now` added to the package's
  dependency manifest.
- `RIO.Test.Clock` module with `newTestClock`: allocates a
  virtual `Clock` whose `now` and `sleep` are driven by an
  explicit `advance :: Milliseconds -> Aff Unit` controller.
  Pending sleepers wake in deadline order within a single
  `advance` call; an interrupted fiber's sleeper is removed
  from the pending list by its canceler and does not fire on
  later advances (Phase 7.1).
- `RIO.Spec` module with `itRIO`, `itRIO_`, and `runSpecRIO`:
  `purescript-spec` integration helpers so an `RIO` program
  slots directly into a `Spec` suite without per-test
  boilerplate. `itRIO` runs a fully-handled `RIO () () Unit`
  via `runRIO'`; `itRIO_` accepts a service record and
  `provideAll`s it before running; `runSpecRIO` pre-installs
  the console reporter and exits the process with the suite's
  result (Phase 7.2). `spec` and `spec-node` added to the
  package's dependency manifest. The trade-off (transitive
  spec dep for every consumer) is intentional for now; we may
  factor `RIO.Spec` out into a sibling workspace package later.
- `docs/07-testing.md`: end-to-end walkthrough of the testing
  story, covering `mockService` and `recording` (from Phase
  2.6), the `Clock` service plus `newTestClock` (Phase 7.1),
  `purescript-spec` integration via `itRIO` / `itRIO_` /
  `runSpecRIO` (Phase 7.2), how to structure tests for layered
  programs (referencing `spikes/phase-5-review/`), and what is
  intentionally absent from Phase 7 (generator-based property
  tests, snapshot testing, per-fiber isolation) (Phase 7.3).
- `spikes/phase-7-review/`: Phase 7 review cycle. Ports the
  Phase 5 review's six-service layered application to a
  `Test.Spec` suite that uses only `RIO.Spec`, `RIO.Test`, and
  `RIO.Test.Clock`. Four scenarios: A. happy path with
  `recording` + `newTestClock`; B. failing layer (`dataLayer`
  raises `dbConnect`); C. program failure after service use
  (typed `progBoom`); D. time-sensitive forks parked on
  `clock.sleep` resumed by `advance` in deadline order.
  Replaces Phase 5's hand-rolled `ScenarioResult` harness with
  ordinary `it` / `itRIO_` bodies and `shouldEqual` assertions.
  All four scenarios pass; CI builds and runs the spike on
  every PR. `FINDINGS.md` records three DX observations:
  `recording` is `Aff Unit`-only (no `recordingWith`); `itRIO_`
  requires `e ~ ()`, so typed-failure inspection falls back to
  plain `it` + `runRIO`; forking a service-using program inside
  a spec body costs an inner `runRIO` because `Aff.forkAff`
  works in `Aff`, not in `RIO`.
- `examples/todo-api/`: Phase 8.1 tutorial example. A small
  HTTP service built on HTTPurple 4.0 plus `rio`. Six modules:
  service interfaces (`Services.purs`), production layer
  wiring (`Layers.purs`) with an in-memory `Ref`-backed store,
  domain handlers (`Handlers.purs`) expressed as `RIO`
  programs over a three-service environment (`logger`,
  `todoStore`, `clock`) with a single typed failure
  (`notFound`), JSON codecs (`Codecs.purs`) bridging
  `argonaut-codecs` to HTTPurple's `JsonEncoder` /
  `JsonDecoder`, route definitions (`Routes.purs`) via
  `Routing.Duplex`, and a bridging `Main.purs` that builds the
  layer once at startup and runs each request via `runRIO`.
  Four endpoints (GET `/todos`, GET `/todos/:id`, POST
  `/todos`, DELETE `/todos/:id`) with HTTP semantics
  (200/204/400/404/405) verified against `curl` end-to-end.
  Persistence is in-memory only; the SQLite-backed variant
  called for in the original build plan is deferred since the
  layer-swap story is already demonstrated by the Phase 7
  review and the example does not need a second driver to
  show off RIO. CI builds the example on every PR.
  `argonaut-codecs`, `argonaut-core`, `httpurple`, and
  `integers` join the example package's dependency manifest;
  none are added to the main `rio` package's dependencies.
- `docs/migrating-from-zio.md` and
  `docs/migrating-from-effect-ts.md`: Phase 8.2 migration
  guides. Each maps idioms 1:1 with code snippets in both
  languages, covering the core type, lifting values,
  composition, services, providing services, typed errors,
  resource safety, concurrency, layers, and testing. Each
  guide closes with a "what RIO does not have yet" backlog
  (STM, `Schedule`, bounded-concurrency `forEach`, tracing /
  metrics, supervisor model, plus `@effect/schema` for the
  Effect-TS guide) and a short "what RIO has that the source
  language does not" section calling out structural error
  rows and `Layer`'s exact-shrink typing.
- Phase 8.3 docstring audit. Every public function across
  `RIO.Core`, `RIO.Env`, `RIO.Error`, `RIO.Concurrency`,
  `RIO.Layer`, `RIO.Resource`, `RIO.Clock`, `RIO.Spec`,
  `RIO.Test`, and `RIO.Test.Clock` now carries at least one
  inline usage example alongside its existing semantics
  description. Specifically added examples to: `unsafeRunRIO`,
  `catchAll`, `mapError`, `die`, `sandbox`, `unsandbox`,
  `fork`, `join`, `interrupt`, `parTraverse`, `parSequence`,
  `zipPar`, `race`, `raceAll`, `fromRecord`, `fromRIO`,
  `unLayer`, `andThen`, `combine`, `buildLayer`,
  `provideLayer`, `acquireRelease`, `addFinalizer`, `scoped`,
  `now`, `sleep`, `liveClock`, `itRIO`, `newTestClock`.
  Publication of the generated docs to Pursuit waits on the
  first registry release.
- `benchmarks/` (workspace package `rio-benchmarks`) plus
  `docs/performance.md`: Phase 8.4 benchmark suite. Four
  scenarios (bind chain at 100 and 10 000 depths, `ask` +
  `Record.get` loop, sequential vs parallel traversal over a
  32-element array of pure work, typed-failure round-trip via
  `fail` + `catchTag`) plus three baselines (`runRIO' pure
  unit`, raw `Aff pure unit`, service-free pure loop). The
  harness is a small `Aff`-aware analogue of `minibench` that
  samples `process.hrtime()` for nanosecond resolution. The
  perf doc records headline numbers on Apple M1 Pro / node 20
  (per-bind cost ~90 ns amortised, service lookup is
  effectively free, `parTraverse` over pure work is ~3x
  sequential, typed-failure round-trip ~930 ns) and the
  reasoning behind the dominant costs. CI builds the suite
  on every PR; it does not run it, since benchmark numbers in
  CI are too noisy to gate on. Setting up a regression gate
  is tracked as a future backlog item.
- `spikes/phase-6-review/`: Phase 6 review cycle. Four randomised
  stress scenarios driven by `Effect.Random` parameters:
  `parTraverse` over up to eight actions with up to 60 percent
  typed-failure rate, `zipPar` with independent failures on each
  side, `raceAll` over up to six branches, and `fork` plus
  `interrupt` against a chain of up to fifty nested `scoped`
  blocks killed mid-sleep. Each iteration asserts the resource
  counter returns to zero. 250 iterations per scenario per
  invocation (1000 total). Across four consecutive local runs
  (4000 total iterations) the harness reports zero leaks across
  every combinator. CI builds and runs the spike on every PR.
  `FINDINGS.md` records three observations: `raceAll` is
  unbiased over its branches in practice, the fork plus
  interrupt path is stable through depth-50 nested scopes, and
  no flaky iterations were observed at the harness's
  millisecond granularity.
- `docs/06-concurrency.md`: walkthrough of the interruption model
  (citing `spikes/aff-interruption/FINDINGS.md` scenarios S1, S2,
  S2b, S3, S4, S5), uninterruptible regions, the cooperative
  cancellation caveat and its `liftAff (delay (Milliseconds 0.0))`
  mitigation, how `race` interacts with `acquireRelease` and
  `Scope`, `parTraverse` failure semantics, and a "what RIO does
  not give you" section calling out the deliberate omissions of
  structured concurrency, interrupt-with-cause, and fiber-local
  state (Phase 6.4).
- `race` and `raceAll` in `RIO.Concurrency` (Phase 6.3). `race`
  uses `Aff`'s `ParAff` `Alt` instance to run two actions
  concurrently, returns whichever completes first (success or
  typed failure), and interrupts the loser. Finalizers registered
  by the loser via `acquireRelease` or `Scope` run on
  interruption, leveraging the same `Aff.bracket` guarantees from
  Phase 0.5 scenario S3. `raceAll` takes a `NonEmptyArray` and is
  the left fold of `race` over the array (no `parOneOf` because
  the `Parallel f m` constraint solver couldn't infer the
  instance from a polymorphic-`f` callsite; the fold is
  equivalent and uses concrete types throughout). `control` and
  `foldable-traversable` added to the dependency manifest.
