# Causes

Both packages distinguish typed failures (`fail`, tracked in the
row) from defects (`die`, surfaced through the underlying
exception channel). Their respective `Cause` modules add a third
shape on top of those: a tree that can record *several* failures
from one program, including failures that happened in parallel
or in sequence.

This doc covers:

1. The `Cause` algebra in each package and when you'd reach for
   it.
2. The cause-aware combinators (predominantly an rio-aff
   surface: `attemptCause`, `bothPar`, `parTraverseCause`,
   `parSequenceCause`, `raceCause`, `acquireReleaseCause`).
3. Rendering: `prettyCause` (both packages) and
   `prettyCauseWithStack` (rio-aff).
4. Why the non-cause combinators stay non-cause by default.

Sources: `rio-fiber/src/RIO/Fiber/Cause.purs` (six-constructor
shape with explicit interruption) and
`rio-aff/src/RIO/Aff/Cause.purs` (four-constructor shape that
folds interruption into `Die`). The `worker-pool` example
exercises `parTraverseCause` + `prettyCause` end-to-end on the
aff side.

## The algebra

### rio-fiber

```purescript
data Cause e
  = Empty                              -- no failure
  | Fail (Variant e)                   -- a typed failure
  | Die Error                          -- a defect
  | Interrupt FiberId                  -- an explicit fiber kill
  | Then (Cause e) (Cause e)           -- two failures, in order
  | Both (Cause e) (Cause e)           -- two failures, same time
```

The shape mirrors ZIO's `Cause` directly: `Empty` is the unit,
`Then` and `Both` carry the structural cases, and `Interrupt`
records a fiber kill by id so an interrupted-only cause can be
distinguished from a defect cause. The `RIO.Fiber.Cause` module
ships predicates (`isEmpty`, `isInterrupted`, `hasDefect`,
`hasFailure`), accessors (`failures`, `defects`, `interrupters`,
`firstFailure`, `firstDefect`, `interruptCount`), strippers
(`stripInterrupts`, `stripFailures`, `stripDefects`), and a
`fold` for cause-shaped recursion. `RIO.Fiber.Error.attemptCause`
reifies an `RIO` outcome into `Either (Cause e) a`.

### rio-aff

```purescript
data Cause e
  = Fail (Variant e)                -- a typed failure
  | Die Error                       -- a defect
  | Parallel (Cause e) (Cause e)    -- two failures, same time
  | Sequential (Cause e) (Cause e)  -- two failures, in order
```

The aff-side algebra collapses interruption into `Die`. `Aff`
distinguishes typed failures from defects, but a fiber kill
lands as an exception too, so the user-visible cause tree
doesn't track interruption as a distinct case. The names
`Parallel` and `Sequential` are exposed where rio-fiber uses
`Both` and `Then`; they mean the same thing.

Atomic causes (`Fail`, `Die`, and on rio-fiber `Interrupt`) are
the atoms the corresponding `Error` modules already expose;
`Parallel` / `Both` and `Sequential` / `Then` let you keep both
when one program raises more than one failure.

A small grammar of *why* causes look this way:

- `parTraverse` and `race` short-circuit on the first failure
  they see. That's the right behavior for fan-out where one
  sibling failing means the whole batch is doomed. It's the
  wrong behavior for "validate ten inputs and tell me
  everything that's wrong". `Parallel` / `Both` is where the
  extra failures go.
- `acquireRelease` and `ensuring` swallow finalizer defects so
  a single bad cleanup doesn't cascade through the rest of the
  scope's finalizers. That's the right tradeoff most of the
  time, but it loses the information that the finalizer *also*
  failed. `Sequential` / `Then` is where that extra failure
  goes when the caller asks for it.

## Reifying outcomes: `attemptCause`

The foundational primitive in both packages. Run an `RIO` and
surface its outcome as `Either (Cause e) a`:

```purescript
attemptCause
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (Either (Cause e) a)
```

A success is `Right a`; a typed failure becomes `Left (Fail v)`;
a defect becomes `Left (Die err)`. On rio-fiber, an interrupted
fiber may surface as `Left (Interrupt fid)` or a tree containing
it. The caller's error row `e'` is left polymorphic because the
outcome lives in the `Either`, not in the channel.

```purescript
outcome <- attemptCause (fetchUser uid)
case outcome of
  Right user -> useUser user
  Left cause -> logCause cause
```

This is what every other `*Cause` combinator builds on.

## Parallel combinators (rio-aff)

The cause-collecting parallel combinators below currently ship
only in rio-aff. On rio-fiber the same observations are
expressible through `attemptCause` + the cause-tree
constructors, but no pre-packaged `bothPar` /
`parTraverseCause` / `raceCause` exists yet.

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
`Parallel cA cB`. Unlike `race` (which surfaces the first
completion) and `Concurrency.Par.ado` (which only keeps the
leftmost typed failure), `bothPar` runs both sides to completion
and reports every failure it sees.

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
keep using `parTraverse`; the costs of cause-tree construction
don't earn their keep when you only need one failure.

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

Unlike the regular `race`, a fast failure does not beat a slow
success. Use it when you want fallback behavior across two
sources of truth and the failure of either alone isn't fatal:

```purescript
result <- raceCause (fromPrimary key) (fromBackup key)
```

If both succeed, the first one to land wins; the loser is
discarded. (Same shape as the regular `race`; the difference
is in the failure path.)

## Sequential composition: `acquireReleaseCause` (rio-aff)

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

## Rendering: `prettyCause` (and rio-aff's `prettyCauseWithStack`)

Both packages export both `prettyCause` and `prettyPrint`. The
implementations are equivalent; rio-fiber defines `prettyPrint`
as the primary and `prettyCause = prettyPrint` as an alias kept
for rio-aff parity, while rio-aff defines both independently.
Pick whichever name reads better for your call site. rio-aff
also exports `prettyCauseWithStack` for defect stack-trace
rendering; rio-fiber has no stack-trace-augmented variant
because its `Die` constructors do not carry a captured stack.

```purescript
prettyCause
  :: forall e. (Variant e -> String) -> Cause e -> String

prettyCauseWithStack         -- rio-aff only
  :: forall e. (Variant e -> String) -> Cause e -> String
```

The caller supplies a renderer for typed failures because
`Variant` does not have a generic `Show`. Defects render via
`Effect.Exception.message`. The output format:

- Atomic causes render on a single line.
- `Parallel` / `Both` introduces a header and indents both
  branches.
- `Sequential` / `Then` introduces a different header so
  readers can tell the two structural cases apart.
- On rio-fiber, `Interrupt fid` renders as its own atomic leaf.

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
  branch, the `Parallel` / `Both` folds, the rendering).
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

| Concept                  | ZIO                            | Effect                  | rio-fiber                 | rio-aff                    |
| ------------------------ | ------------------------------ | -------------------------- | ----------------------------- | ------------------------------ |
| Cause data type          | `Cause[E]`                     | `Cause<E>`                 | `Cause e`                     | `Cause e`                      |
| Typed failure leaf       | `Cause.Fail`                   | `Cause.Fail`               | `Fail (Variant e)`            | `Fail (Variant e)`             |
| Defect leaf              | `Cause.Die`                    | `Cause.Die`                | `Die Error`                   | `Die Error`                    |
| Interrupt leaf           | `Cause.Interrupt`              | `Cause.Interrupt`          | `Interrupt FiberId`           | (folded into `Die`)            |
| Empty                    | `Cause.Empty`                  | (n/a as constructor)       | `Empty`                       | (n/a as constructor)           |
| Parallel composition     | `Cause.Both`                   | `Cause.Parallel`           | `Both`                        | `Parallel`                     |
| Sequential composition   | `Cause.Then`                   | `Cause.Sequential`         | `Then`                        | `Sequential`                   |
| Reify outcome            | `ZIO.attemptCause`             | `Effect.exit`              | `attemptCause`                | `attemptCause`                 |
| Collect failures         | `ZIO.collectAllPar` + sandbox  | `Effect.allCause`          | (via `attemptCause`)          | `parTraverseCause`             |
| Race waiting for success | `ZIO.raceWith`                 | `Effect.race` + sandbox    | (via `attemptCause`)          | `raceCause`                    |
| Cause-aware bracket      | `ZIO.acquireReleaseExitCause`  | `Effect.acquireExit`       | (via `attemptCause` + bracket)| `acquireReleaseCause`          |
| Render to text           | `Cause.prettyPrint`            | `Cause.pretty`             | `prettyCause` / `prettyPrint` | `prettyCause` / `prettyCauseWithStack` |

rio-fiber matches ZIO's six-constructor algebra one-for-one and
keeps interruption as a distinct leaf, which is what makes
`isInterruptedOnly` and friends useful when distinguishing a
clean cancellation from a defect. rio-aff trades that distinction
for the smaller four-constructor algebra because `Aff` does not
expose a structured interruption signal at the user level.

## Pointers

- Sources:
  [`rio-fiber/src/RIO/Fiber/Cause.purs`](../rio-fiber/src/RIO/Fiber/Cause.purs)
  and
  [`rio-aff/src/RIO/Aff/Cause.purs`](../rio-aff/src/RIO/Aff/Cause.purs).
- Spec coverage:
  [`rio-fiber/test/Test/RIO/Fiber/CauseSpec.purs`](../rio-fiber/test/Test/RIO/Fiber/CauseSpec.purs)
  and
  [`rio-aff/test/Test/RIO/Aff/CauseSpec.purs`](../rio-aff/test/Test/RIO/Aff/CauseSpec.purs).
- Worked example: [`examples/worker-pool/`](../examples/worker-pool/)
  uses `parTraverseCause` + `prettyCause` for a pre-flight
  validation pass on the aff side.
- Related: [`docs/03-errors.md`](./03-errors.md) for the
  typed-failure / defect distinction the leaves sit on top of.
