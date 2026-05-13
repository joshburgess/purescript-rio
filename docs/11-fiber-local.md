## Fiber-local state

`RIO.Local` is the primitive for ambient state that is
read by most callers and overridden for the duration of a block
by a few. The classic use cases are:

- correlation / request IDs that thread through every log line
  and span without explicit plumbing;
- request-scoped config overrides (a log level, a tenant ID, a
  feature flag override) that apply only within one handler;
- the "current span" context that `RIO.Tracer` already does
  internally, exposed for your own ambient values.

```purescript
import RIO.Local (Local, get, locally, newLocal)

type Env =
  { logger :: Logger
  , requestId :: Local String
  }

handleRequest
  :: forall e
   . Request
  -> RIO Env e Response
handleRequest req = locally (asRequestId env) req.id do
  -- everything inside this block sees req.id as the
  -- request ID; after the block exits, the previous value
  -- (typically a placeholder set at startup) is restored.
  logSomething
  callDownstream
```

The shape mirrors ZIO's `FiberRef` and the `Context`-based
fiber state in Effect-TS. The semantics on our `Aff`-based
runtime differ; see "Inheritance and concurrency" below.

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
  `update` is `Ref.modify_` under the hood.
- `locally fl value action` runs `action` with the cell set to
  `value`, then restores the previous value. The restore is
  guaranteed by `Aff.finally`, so it runs on every termination
  path: success, typed failure, defect, fiber interruption
  mid-action.

## Inheritance and concurrency

A `Local a` is backed by an `Effect.Ref`. Every fiber that
holds the same `Local` reference (typically: every fiber in
the same environment row) reads from and writes to the same
cell. This has two consequences worth knowing:

### Forks inherit the *current* value, not a snapshot

Inside a `locally` block, a forked child fiber that reads the
cell sees the override, because the override is what's
currently in the cell. After the parent's `locally` exits the
restore happens; subsequent reads from any fiber, including a
child that started inside `locally`, see the restored value.

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

### Writes from any fiber are visible everywhere

`set` and `update` write to the shared `Ref`. A sibling fiber
that reads the cell sees the new value immediately. This is
*not* the per-fiber isolation that ZIO's `FiberRef` provides.
For the common patterns (snapshot once at the top, read-only
downstream) it does not matter; for genuinely concurrent
mutation across sibling fibers it does, and you should reach
for `RIO.STM` or a `RIO.Deferred` instead of a `Local`.

This trade-off is the same one `RIO.Tracer` makes for its
implicit parent / child context. The PureScript runtime here
is `Aff`, and `Aff` does not expose fork hooks we could use
to instrument per-fiber snapshotting. The simpler model is
clearly documented; if a future iteration wires fork-time
capture into `RIO.Concurrency.fork`, we'll lift `Local` to
match.

## Nesting

`locally` blocks compose cleanly. Each inner `locally`
restores to whatever value its enclosing `locally` was using,
not to the original:

```purescript
locally tier "free" do
  -- reads of `tier` see "free"
  locally tier "pro" do
    -- reads of `tier` see "pro"
  -- back to "free", not the initial default
```

## Comparison to ZIO and Effect-TS

| Concept                    | RIO                            | ZIO                              | Effect-TS                          |
| -------------------------- | ------------------------------ | -------------------------------- | ---------------------------------- |
| Create a cell              | `newLocal value`               | `FiberRef.make(value)`           | `FiberRef.make(value)`             |
| Read                       | `get fl`                       | `fl.get`                         | `FiberRef.get(fl)`                 |
| Write                      | `set fl value`                 | `fl.set(value)`                  | `FiberRef.set(fl, value)`          |
| Scoped override            | `locally fl value action`      | `fl.locally(value)(action)`      | `Effect.locally(fl, value)(self)`  |
| Per-fiber isolation        | *no* (shared `Ref`)            | yes                              | yes                                |
| Fork inheritance           | the current value (shared)     | a snapshot copy                  | a snapshot copy                    |
| Restore on every exit      | yes (`Aff.finally`)            | yes                              | yes                                |

The single behavioural difference is the snapshot-vs-shared
fork semantics. If you find yourself wanting true per-fiber
isolation, file an issue with the use case; until then
`RIO.Local` covers the ambient-state-with-scope pattern at
about a dozen lines of implementation.

## Pointers

- `src/RIO/Local.purs`: the module.
- `test/Test/RIO/LocalSpec.purs`: tests for `get` / `set` /
  `update`, `locally` restore on success and on typed failure,
  nested `locally`, and the documented fork-inheritance
  semantics (child sees parent's current value; child writes
  are visible to the parent).
