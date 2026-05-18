# Future Work: Gaps vs ZIO / Effect

Potential future work for `purescript-rio`. The core demonstration
is complete; what remains here is additive, deferred pending a
concrete use case, or pending upstream constraints.

## Open items

### `rio-aws` integration package

A typed wrapper around the AWS SDK v3 client surface, exposing
services (S3, SQS, DynamoDB, etc.) as RIO service rows with
`Cause`-aware error variants. Equivalent in scope to early ZIO AWS
or `@effect/aws-client`. The surface is large (one service row per
client) and the work is additive rather than core demonstration
material.

### Friendlier missing-tag error for `catchTag`

The payload-type-mismatch case (case 03) was polished in
`RIO.Error` via a row-list-keyed `FindErrorTag` / `CatchableErrorTag`
walk, promoting it from ACCEPTABLE-NOISY to GOOD. The missing-tag
case (case 04) still surfaces the underlying `Prim.Row.Cons`
row-mismatch because the constraint required for the residual-row
calculation fires its error first at the use site, shadowing the
custom Fail attached to `FindErrorTag`'s `Nil` instance. Two
restructures were investigated and ruled out: folding `Row.Cons`
inside the `CatchableErrorTag` instance head (the mismatch still
wins) and replacing `Row.Cons` with a row-list walk plus
`FromRowList` rebuild (regresses open-row call sites because
`RowToList` is closed-only). Pending an upstream change to
`Prim.Row.Cons` error reporting or a different encoding of
typed-error rows, the residual jargon stays. Tracked in
`compile-fail/FINDINGS.md`.

### Config rotation triggers

`RIO.Config.Rotating` ships `newRotating`, `readRotating`,
`writeRotating`, and `withRotation`. Polling / signal-triggered
rotation is left to the caller; a polling helper or signal-based
trigger could land if a clear pattern emerges from real use.

## Out of scope for the core demonstration

These are real features in ZIO / Effect but adding any of them
would weaken the message rather than strengthen it. They're
non-goals for the "is this real" milestone.

- Full `ZStream` parity (sinks, parallel streams, transducers
  beyond what `RIO.Stream` / `RIO.Stream.Par` / `RIO.Sink` /
  `RIO.Channel` already cover)
- Kafka / Redis / MongoDB adapters
- Deeper transactional STM features beyond what's already shipped
  (`atomically`, `retry`, `check`, `orElse`, `failSTM`), e.g.
  nested transactions. (`TVar` ships as an alias for `TRef` for
  muscle memory; there is no separate distinct type.)
- A custom runtime / fiber supervisor beyond what `Aff` provides
- A web framework on top of `rio-http` (HTTPurple is enough for the
  examples)
- Cron / scheduled-job adapter (Schedule covers backoff; cron is a
  separate concern)
- Auto-derived persistent storage / ORM features
