## `RIO.Sink` design notes

This doc records *why* `RIO.Sink` looks the way it does. The
user-facing reference (constructors, primitives, combinators,
runner) lives in [`docs/13-streams.md`](./13-streams.md) under
"Composable consumers". Read that first if you just want to
know what's in the module; come here for the rationale.

The headline decisions:

- `Sink r e i a` is a `RIO r e (Step r e i a)` with
  `Step = Need (i -> Sink r e i a) (RIO r e a) | Halt a`,
  not the simpler ZIO 1.x `Need a (i -> Sink)` shape.
- `zipPar` is single-fiber and pulls each input once. The
  per-fiber, per-consumer-queue variant is
  `RIO.Stream.Par.broadcast`.
- No full `Channel` algebra; no push-based variant.
- No `Sink.fromQueue` / `Sink.fromHub` family yet.

The rest of this note explains each call.

## Why `Need k finish` rather than `Need a k`

The first sketch had `Need a (i -> Sink r e i a)`: every step
carries a pure default `a` to return if the stream ends now.
That shape worked for `count`, `foldL`, `find`, and `take`, but
it broke `andThen`.

`andThen` needs to thread the first sink's result through to the
second. With `Need a k`, when the upstream stream ends we have
to feed the first sink's pure default into the continuation,
even if the continuation needs to do work (run a finalizer,
read an effect, raise on the error row). The only escape was
`unsafePartialDefault`-style typeclass hackery to materialise an
arbitrary `a`. The result was a foreign import smell for what
should be a tidy combinator.

Switching `finish` from `a` to `RIO r e a` removes the problem
entirely: end-of-stream handling is itself effectful. `andThen`
now reads as "if the first sink is still consuming, run its
`finish`, feed the result into `k`, then run `k`'s sink against
`Stream.empty`". No partial defaults, no foreign imports, no
hidden assumptions about the result shape. Every primitive in
the module already had a perfectly good `RIO r e a` to supply
for `finish` (often just `pure acc`), so the cost at every
existing call site was zero.

Conduit and ZIO 1.x's `ZSink` both used the simpler `Need a k`
shape because their host languages let them play looser with
end-of-stream behavior. PureScript's row-typed errors make the
effectful-`finish` variant cleaner.

## Why `zipPar` is single-fiber

ZIO and Effect-TS both expose a fan-out primitive that hands
the same stream to N concurrent consumers, each on its own
fiber, with backpressure between them. In this library that's
already `RIO.Stream.Par.broadcast`.

`zipPar` is deliberately different: it runs two sinks in
lockstep on **the same fiber**, pulling each input exactly
once and offering it to both step functions before the next
pull. The trade-off:

- `zipPar` is cheaper (no fibers, no queues, no per-consumer
  buffer) and preserves first-failure-wins from the underlying
  `RIO.Stream.Par` semantics without spinning up siblings.
- `broadcast` is what you want when the two consumers have
  asymmetric throughput and you want backpressure to hold the
  slow one back without stalling the fast one.

A library that only offered one would force the wrong shape on
half the use cases. The example in `examples/sink-analytics/`
is a `zipPar` use case (five small aggregations, all fast,
share one stream pass); `examples/stream-pipeline/` is a
`broadcast` use case (a logger and a metrics aggregator with
different cost models). Naming them differently and shipping
both is cheaper than picking one and trying to make it serve
the other.

`zipPar`'s implementation is `combineSteps :: Step r e i a ->
Step r e i b -> Step r e i (Tuple a b)`. The four cases
(`Halt × Halt`, `Halt × Need`, `Need × Halt`, `Need × Need`)
each preserve the natural invariant: once a side halts, its
final value is remembered and only the other side continues to
see inputs; both `finish` actions run on stream exhaustion and
their results are tupled.

## Why no full `Channel` algebra

ZIO's `Channel` unifies streams, sinks, and pipes into one
six-parameter algebra. The unification has clear theoretical
appeal: every transducer is a Channel, every parallel
combinator is a Channel composition, and the Sink / Stream
distinction becomes a row-of-types convention rather than two
separate datatypes.

It pays for that with surface area that's hard for a casual
reader to inspect. The user-facing type signatures grow six
type parameters wide. The combinator names overlap with both
Stream and Sink. The fusion story is a separate body of work
on top.

`RIO.Sink` covers the real terminating-consumer cases without
the Channel framing. The transducer cases that *would* require
Channel (a stream-to-stream component with both consumer and
producer behavior) are already covered by `Stream.mapM`,
`Stream.flatMap`, and `Sink.andThen` for the common patterns.
The threshold for revisiting Channel is a concrete use case
that this trio cannot express, not "ZIO has it."

## Why no push-based variant

The library's `Stream` is pull-based: the consumer asks for
the next value, the producer hands it over. `Sink` is the
matching pull-based consumer.

A push-based `Stream` would be a different design conversation
(different cost model, different fusion story, different
interaction with `scoped` and backpressure). Adding both
shapes side-by-side would double the surface area for a
benefit that does not show up in the current example set. If
real pressure for push-based shapes appears, it should drive
its own design pass rather than be retro-fitted onto this one.

## What's deliberately not shipped (yet)

- `Sink.fromQueue` / `Sink.fromHub` family. Once `Sink` exists,
  these are 5-line aliases over `Sink.foldM` pointing at the
  `RIO.Queue` / `RIO.Hub` modules. Ship them in a follow-up
  when an example actually needs them; until then they add
  surface area without earning their keep.
- Sink-side fusion / rewriting. Today every `mapResult`,
  `mapInput`, and `filterIn` allocates a fresh wrapper per
  step. A real workload that shows up in a profile is the
  right driver for a fusion pass, not a speculative one.
- Channel (see above).

## Pointers

- User-facing reference: [`docs/13-streams.md`](./13-streams.md)
  ("Composable consumers" section).
- Source: [`src/RIO/Sink.purs`](../src/RIO/Sink.purs).
- Worked example: [`examples/sink-analytics/`](../examples/sink-analytics/).
- Spec coverage: [`test/Test/RIO/SinkSpec.purs`](../test/Test/RIO/SinkSpec.purs).
