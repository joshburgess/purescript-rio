## Fiber-local state

> **Naming convention.** This guide covers two distinct
> implementations: `RIO.Aff.Local` (backed by a shared
> `Effect.Ref`, no per-fiber isolation) and `RIO.Fiber.Local`
> (backed by a `FiberRef`, true per-fiber snapshot semantics).
> The two share the same surface (`newLocal`, `get`, `set`,
> `update`, `locally`) but differ in concurrency semantics.
> Both are described below, with explicit "rio-aff:" /
> "rio-fiber:" callouts at every divergence.

`Local` is the primitive for ambient state that is read by most
callers and overridden for the duration of a block by a few.
The classic use cases are:

- correlation / request IDs that thread through every log line
  and span without explicit plumbing;
- request-scoped config overrides (a log level, a tenant ID, a
  feature flag override) that apply only within one handler;
- the "current span" context that `RIO.Aff.Tracer` /
  `RIO.Fiber.Tracer` already does internally, exposed for your
  own ambient values.

```purescript
import RIO.Aff.Local (Local, get, locally, newLocal)
-- rio-fiber: import RIO.Fiber.Local (Local, get, locally, newLocal)

type Env =
  { logger :: Logger
  , requestId :: Local String
  }

handleRequest
  :: forall e
   . Request
  -> RIO Env e Response
handleRequest req = do
  requestId <- asks _.requestId
  locally requestId req.id do
    -- everything inside this block sees req.id as the
    -- request ID; after the block exits, the previous value
    -- (typically a placeholder set at startup) is restored.
    logSomething
    callDownstream
```

The shape mirrors ZIO's `FiberRef` and the `Context`-based
fiber state in Effect. `RIO.Fiber.Local` matches their
semantics exactly; `RIO.Aff.Local` provides the same surface
but with shared-Ref semantics under forks, as explained below.

## API

```purescript
newLocal :: forall r e' a. a -> RIO r e' (Local a)
newLocalEffect :: forall a. a -> Effect (Local a)

get :: forall r e a. Local a -> RIO r e a
set :: forall r e a. Local a -> a -> RIO r e Unit
update :: forall r e a. Local a -> (a -> a) -> RIO r e Unit

locally :: forall r e a b. Local a -> a -> RIO r e b -> RIO r e b
```

- `newLocal` (or `newLocalEffect` for callers that build their
  environment outside an `RIO` action) creates a fresh cell
  initialised to a default value.
- `get`, `set`, `update` are the basic read/write operations.
- `locally fl value action` runs `action` with the cell set to
  `value`, then restores the previous value on every
  termination path: success, typed failure, defect, fiber
  interruption mid-action. (rio-aff: restore is guaranteed by
  `Aff.finally`; rio-fiber: restore is guaranteed by the
  runtime's `ensuring` finalizer.)

## Inheritance and concurrency

This is the section where the two packages diverge.

### rio-fiber: `FiberRef`-backed, true per-fiber isolation

`RIO.Fiber.Local` is a thin newtype over `FiberRef`. The fiber
runtime guarantees:

- **Forks copy a snapshot.** When a fiber forks a child, the
  child starts with a snapshot of the parent's current value.
  Subsequent writes in either fiber are invisible to the other.
- **`locally` writes only affect the current fiber.** The
  override is applied to the calling fiber's view; sibling
  fibers continue to see their own snapshots.
- **Nested `locally` restores to the enclosing block's value**,
  not to the original. This is the standard ZIO / Effect
  behaviour.

This is the "fiber-local" model in the strict sense and matches
ZIO `FiberRef` and Effect `FiberRef.locally` exactly.

### rio-aff: shared `Effect.Ref`, no per-fiber isolation

`RIO.Aff.Local` is backed by an `Effect.Ref`. Every fiber that
holds the same `Local` reference (typically: every fiber in
the same environment row) reads from and writes to the same
cell. This has two consequences worth knowing.

**Forks inherit the *current* value, not a snapshot.** Inside a
`locally` block, a forked child fiber that reads the cell sees
the override, because the override is what's currently in the
cell. After the parent's `locally` exits the restore happens;
subsequent reads from any fiber, including a child that started
inside `locally`, see the restored value.

That's usually what you want for the common case: snapshot a
correlation ID at the top of a request, run any number of
fibers under it, and join them before the request handler
returns. As long as your `locally` body awaits its forked
fibers (with `join`, `parTraverse`, etc.), every child sees
the override for the whole of its lifetime.

It is *not* what you want if a child fiber is supposed to keep
operating on its own private copy after the parent's `locally`
has exited. That pattern requires capturing the value
explicitly at the fork point:

```purescript
locally requestId req.id do
  snapshot <- get requestId
  void $ fork do
    -- this fiber may outlive the locally block;
    -- pretend it has its own local cell:
    privateRid <- newLocal snapshot
    longRunningWork privateRid
```

Or, if you need true per-fiber isolation throughout the
program, switch to the rio-fiber package and use
`RIO.Fiber.Local`.

**Writes from any fiber are visible everywhere.** `set` and
`update` write to the shared `Ref`. A sibling fiber that reads
the cell sees the new value immediately. This is *not* the
per-fiber isolation that ZIO's `FiberRef` provides. For the
common patterns (snapshot once at the top, read-only
downstream) it does not matter; for genuinely concurrent
mutation across sibling fibers it does, and you should reach
for `RIO.Aff.STM` or a `RIO.Aff.Deferred` instead of a `Local`.

This trade-off is the same one `RIO.Aff.Tracer` makes for its
implicit parent / child context. The PureScript runtime here is
`Aff`, and `Aff` does not expose fork hooks we could use to
instrument per-fiber snapshotting. rio-fiber's custom
interpreter does provide those hooks, which is why
`RIO.Fiber.Local` can offer the strict per-fiber model.

## Nesting

`locally` blocks compose cleanly under both runtimes. Each
inner `locally` restores to whatever value its enclosing
`locally` was using, not to the original:

```purescript
locally tier "free" do
  -- reads of `tier` see "free"
  locally tier "pro" do
    -- reads of `tier` see "pro"
  -- back to "free", not the initial default
```

## Comparison to ZIO and Effect

| Concept                | rio-aff `Local`         | rio-fiber `Local`        | ZIO                       | Effect                        |
| ---------------------- | ----------------------- | ------------------------ | ------------------------- | ----------------------------- |
| Create a cell          | `newLocal value`        | `newLocal value`         | `FiberRef.make(value)`    | `FiberRef.make(value)`        |
| Read                   | `get fl`                | `get fl`                 | `fl.get`                  | `FiberRef.get(fl)`            |
| Write                  | `set fl value`          | `set fl value`           | `fl.set(value)`           | `FiberRef.set(fl, value)`     |
| Scoped override        | `locally fl v action`   | `locally fl v action`    | `fl.locally(v)(action)`   | `Effect.locally(fl, v)(self)` |
| Per-fiber isolation    | *no* (shared `Ref`)     | yes (`FiberRef`)         | yes                       | yes                           |
| Fork inheritance       | current value (shared)  | snapshot copy            | snapshot copy             | snapshot copy                 |
| Restore on every exit  | yes (`Aff.finally`)     | yes (`ensuring`)         | yes                       | yes                           |

The shared-vs-snapshot fork semantics is the one behavioural
difference between the two RIO runtimes. If you need true
per-fiber isolation, `RIO.Fiber.Local` is the answer; if you
only need ambient-state-with-scope and your fibers are
joined before the enclosing block exits, `RIO.Aff.Local`
covers it at about a dozen lines of implementation.

## Pointers

- `rio-aff/src/RIO/Aff/Local.purs`: shared-Ref implementation.
- `rio-fiber/src/RIO/Fiber/Local.purs`: `FiberRef`-backed
  implementation with per-fiber snapshot semantics.
- `rio-aff/test/Test/RIO/Aff/LocalSpec.purs`: tests for `get` /
  `set` / `update`, `locally` restore on success and on typed
  failure, nested `locally`, and the documented
  fork-inheritance semantics (child sees parent's current
  value; child writes are visible to the parent).
- The rio-fiber `FiberRef` semantics are exercised by the
  fiber runtime's own fork / interrupt test suite.
