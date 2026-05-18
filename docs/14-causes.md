# Causes

`RIO.Error` distinguishes typed failures (`fail`, tracked in
the row) from defects (`die`, surfaced through the underlying
`Aff` exception channel). `RIO.Cause` adds a third shape on top
of those: a tree that can record *several* failures from one
program, including failures that happened in parallel or in
sequence.

This doc covers:

1. The `Cause` algebra and when you'd reach for it.
2. The cause-aware combinators: `attemptCause`, `bothPar`,
   `parTraverseCause`, `parSequenceCause`, `raceCause`,
   `acquireReleaseCause`.
3. Rendering: `prettyCause` and `prettyCauseWithStack`.
4. Why the non-cause combinators stay non-cause by default.

The source is `src/RIO/Cause.purs`. The `worker-pool` example
exercises `parTraverseCause` + `prettyCause` end-to-end.

## The algebra

```purescript
data Cause e
  = Fail (Variant e)                -- a typed failure
  | Die Error                        -- a defect
  | Parallel (Cause e) (Cause e)    -- two failures, same time
  | Sequential (Cause e) (Cause e)  -- two failures, in order
```

Atomic causes (`Fail`, `Die`) are the same atoms `RIO.Error`
already exposes; `Parallel` and `Sequential` let you keep both
when one program raises more than one failure.

A small grammar of *why* causes look this way:

- `parTraverse` and `race` short-circuit on the first failure
  they see. That's the right behavior for fan-out where one
  sibling failing means the whole batch is doomed. It's the
  wrong behavior for "validate ten inputs and tell me
  everything that's wrong". `Parallel` is where the extra
  failures go.
- `acquireRelease` and `ensuring` swallow finalizer defects so
  a single bad cleanup doesn't cascade through the rest of the
  scope's finalizers. That's the right tradeoff most of the
  time, but it loses the information that the finalizer *also*
  failed. `Sequential` is where that extra failure goes when
  the caller asks for it.

## Reifying outcomes: `attemptCause`

The foundational primitive. Run an `RIO` and surface its
outcome as `Either (Cause e) a`:

```purescript
attemptCause
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (Either (Cause e) a)
```

A success is `Right a`; a typed failure becomes `Left (Fail v)`;
a defect becomes `Left (Die err)`. The caller's error row `e'`
is left polymorphic because the outcome lives in the `Either`,
not in the channel.

```purescript
outcome <- attemptCause (fetchUser uid)
case outcome of
  Right user -> useUser user
  Left cause -> logCause cause
```

This is what every other `*Cause` combinator builds on.

## Parallel combinators

### `bothPar`

Run two `RIO`s in parallel; surface both failures if both fail:

```purescript
bothPar
  :: forall r e a b
   . RIO r e a
  -> RIO r e b
  -> RIO r e (Either (Cause e) (Tuple a b))
```

If both succeed, `Right (Tuple a b)`. If exactly one fails, the
failing side's cause is returned. If both fail, the result is
`Parallel cA cB`. Unlike `RIO.Concurrency.race` (which surfaces
the first completion) and `Concurrency.Par.ado` (which only
keeps the leftmost typed failure), `bothPar` runs both sides to
completion and reports every failure it sees.

### `parTraverseCause` / `parSequenceCause`

The collecting variants of `parTraverse` / `parSequence`:

```purescript
parTraverseCause
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Either (Cause e) (Array b))

parSequenceCause
  :: forall r e e' a
   . Array (RIO r e a)
  -> RIO r e' (Either (Cause e) (Array a))
```

Every branch runs to completion under `attempt`. If anything
fails, every failure is folded into a left-leaning `Parallel`
tree. If everything succeeds, the results come back in input
order.

This is the combinator the renderer was written for: reach for
it when "tell me everything that broke" matters more than
"tell me as fast as possible". For first-failure-wins fan-out,
keep using `RIO.Concurrency.parTraverse`; the costs of
cause-tree construction don't earn their keep when you only
need one failure.

The `worker-pool` example uses `parTraverseCause` for a
pre-flight pass that validates every job before any of them
runs, then renders the combined failure with `prettyCause`.

### `raceCause`

The cause-aware race: wait for the **first success**. Only
return failure when both sides have failed, and combine both
causes into a `Parallel` tree:

```purescript
raceCause
  :: forall r e e' a
   . RIO r e a
  -> RIO r e a
  -> RIO r e' (Either (Cause e) a)
```

Unlike `RIO.Concurrency.race`, a fast failure does not beat a
slow success. Use it when you want fallback behavior across
two sources of truth and the failure of either alone isn't
fatal:

```purescript
result <- raceCause (fromPrimary key) (fromBackup key)
```

If both succeed, the first one to land wins; the loser is
discarded. (Same shape as the regular `race`; the difference
is in the failure path.)

## Sequential composition: `acquireReleaseCause`

Cause-aware bracket. When the body fails *and* the finalizer
also fails, the result is a `Sequential` cause that pairs both:

```purescript
acquireReleaseCause
  :: forall r e e' a b
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> (a -> RIO r e b)
  -> RIO r e' (Either (Cause e) b)
```

Behaviour:

- Acquire failure: a single `Fail` / `Die` cause; nothing else
  runs.
- Body fails, finalizer succeeds: the body's cause is returned
  alone.
- Body succeeds, finalizer fails: the finalizer's cause is
  returned alone.
- Both fail: `Sequential body release` records the pair.

The finalizer still runs in the uninterruptible release phase
of `Aff.bracket`; a fiber kill landing during the body is
queued until release completes.

## Rendering: `prettyCause` and `prettyCauseWithStack`

```purescript
prettyCause
  :: forall e. (Variant e -> String) -> Cause e -> String

prettyCauseWithStack
  :: forall e. (Variant e -> String) -> Cause e -> String
```

The caller supplies a renderer for typed failures because
`Variant` does not have a generic `Show`. Defects render via
`Effect.Exception.message`. The output format:

- Atomic causes render on a single line.
- `Parallel` introduces a header and indents both branches.
- `Sequential` introduces a different header so readers can
  tell the two structural cases apart.

`prettyCauseWithStack` is the same as `prettyCause` except each
`Die` leaf shows the JS stack trace underneath its message
(when one is available). Each stack line is indented one level
deeper than the `defect:` header so the tree structure stays
readable.

```purescript
case outcome of
  Right _ -> ...
  Left cause -> liftEffect
    (Console.log (prettyCauseWithStack renderTypedFailure cause))
```

## Why the non-cause primitives stay non-cause

`race`, `parTraverse`, `acquireRelease`, and `ensuring` all
have non-cause shapes that match the simple first-failure /
swallow-finalizer-defect behavior. The cause-aware variants
live alongside them rather than replacing them, on purpose:

- Cause tree construction has a real cost (`attempt` on every
  branch, the `Parallel` / `Sequential` folds, the rendering).
  Programs that don't need the extra information shouldn't pay
  for it.
- The non-cause shapes match common idioms one-to-one. A user
  who reads `parTraverse` from the ZIO ecosystem expects
  first-failure-wins; the cause-collecting variant should be a
  separate name so the surface stays predictable.
- If the trade-off ever flips (cause-collecting becomes the
  desired default), the migration is mechanical: switch the
  implementations over to the `*Cause` variants and surface
  the cause through a new service row, the way ZIO and
  Effect do.

## Comparison with ZIO / Effect

| Concept                  | ZIO                            | Effect                  | `purescript-rio`             |
| ------------------------ | ------------------------------ | -------------------------- | ---------------------------- |
| Cause data type          | `Cause[E]`                     | `Cause<E>`                 | `Cause e`                    |
| Typed failure leaf       | `Cause.Fail`                   | `Cause.Fail`               | `Fail (Variant e)`           |
| Defect leaf              | `Cause.Die`                    | `Cause.Die`                | `Die Error`                  |
| Parallel composition     | `Cause.Both`                   | `Cause.Parallel`           | `Parallel`                   |
| Sequential composition   | `Cause.Then`                   | `Cause.Sequential`         | `Sequential`                 |
| Reify outcome            | `ZIO.attemptCause`             | `Effect.exit`              | `attemptCause`               |
| Collect failures         | `ZIO.collectAllPar` + sandbox  | `Effect.allCause`          | `parTraverseCause`           |
| Race waiting for success | `ZIO.raceWith`                 | `Effect.race` + sandbox    | `raceCause`                  |
| Cause-aware bracket      | `ZIO.acquireReleaseExitCause`  | `Effect.acquireExit`       | `acquireReleaseCause`        |
| Render to text           | `Cause.prettyPrint`            | `Cause.pretty`             | `prettyCause` / `prettyCauseWithStack` |

The chief simplification relative to ZIO: there's no
`Cause.Interrupt` constructor. Fiber kills surface as `Aff`
exceptions and land as `Die err` leaves; the user-visible cause
tree doesn't track interruption as a distinct case. That keeps
the algebra to four constructors instead of five and matches
what `Aff` actually distinguishes at runtime.

## Pointers

- Source: [`src/RIO/Cause.purs`](../src/RIO/Cause.purs).
- Spec coverage: [`test/Test/RIO/CauseSpec.purs`](../test/Test/RIO/CauseSpec.purs).
- Worked example: [`examples/worker-pool/`](../examples/worker-pool/)
  uses `parTraverseCause` + `prettyCause` for a pre-flight
  validation pass.
- Related: [`docs/03-errors.md`](./03-errors.md) for the
  typed-failure / defect distinction the leaves sit on top of.
