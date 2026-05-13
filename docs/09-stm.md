# Software Transactional Memory

`RIO.STM` is the transactional-state primitive for `RIO`. An
`STM e a` is a pure description of a transaction; `atomically`
runs it as an `RIO` action that either commits every staged
write at once or applies none.

The shape mirrors ZIO `STM` / Effect-TS `STM`:

- `TRef a`: a transactional reference, the unit of shared state.
- `STM e a`: the transaction monad. Reads, writes, and decisions
  go in here; everything else stays out.
- `atomically :: STM e a -> RIO r e a`: the bridge. Inside a
  transaction you see one consistent snapshot; outside one you
  see committed values only.

## The atomicity story

In a multi-threaded runtime an STM implementation needs
optimistic concurrency control: a transaction reads a value, does
some work, then on commit verifies that nothing has changed
underneath it. PureScript runs on JavaScript's single event loop,
so the picture is simpler:

1. The body of `atomically` is a synchronous `Effect`.
2. No other fiber can run during a synchronous `Effect`.
3. Therefore no other fiber can observe (or change) the
   transaction's intermediate writes.

The `STM` implementation in `RIO.STM` exploits that directly. A
transaction accumulates writes in a log; the commit phase applies
them to the underlying `Ref`s in a single `Effect` block, then
fires the waiters registered on each written `TRef`. No version
numbers, no rollback machinery, no spinning.

The trade-off is the same one PureScript's runtime makes
everywhere: an STM body that allocates a fiber, awaits an `Aff`,
or otherwise hits an async boundary is *not* what `atomically`
expects. The `STM` monad has no `MonadAff` instance for this
reason. If you need to talk to the outside world inside a
transaction's logic, do the I/O *outside* `atomically` and pass
the result in.

## Building blocks

```purescript
newTRef    :: forall e a. a -> STM e (TRef a)
readTRef   :: forall e a. TRef a -> STM e a
writeTRef  :: forall e a. TRef a -> a -> STM e Unit
modifyTRef :: forall e a. TRef a -> (a -> a) -> STM e Unit
```

A `TRef`'s identity is the underlying `Ref`: two `TRef`s built
from separate `newTRef` calls are distinct, and writes do not
clash. `modifyTRef` is `readTRef` then `writeTRef`; pulling it
out as a primitive is just for readability.

`atomically` runs one transaction:

```purescript
atomically :: forall r e a. STM e a -> RIO r e a
```

A single transaction observes a consistent snapshot. Across
`atomically` calls, no guarantee: state can change between calls.
If you need read-then-write consistency, put both in one
`atomically`.

## `retry` and `check`

`retry` aborts the current attempt and re-runs the transaction
once any `TRef` it read has changed. `check b` is `retry` when
`b` is `false` and a no-op when it's `true`:

```purescript
-- block until the counter goes positive, then decrement it
takeOne :: TRef Int -> STM () Int
takeOne counter = atomically' do
  n <- readTRef counter
  check (n > 0)
  writeTRef counter (n - 1)
  pure n
```

A retried transaction registers waiter callbacks on every `TRef`
it read. The waiters fire when any of those refs is *written*,
even if the new value would still cause the transaction to
retry. (Reads aren't "smart" about value changes; only writes
trigger a wake.)

A transaction that retries with an *empty* read log will deadlock:
nothing can ever wake it. `atomically` does not detect this; it
is the caller's responsibility.

## Typed failures inside a transaction

`failSTM` raises a typed failure on the STM's error row:

```purescript
failSTM
  :: forall sym a v e e1
   . Cons sym v e1 e
  => IsSymbol sym
  => Proxy sym
  -> v
  -> STM e a
```

A failed transaction aborts and the failure surfaces on the
parent `RIO`'s error row. Writes staged before the failure are
*discarded*, exactly as if the transaction never ran.

This is the bridge from `STM`'s error channel to `RIO`'s:

```purescript
data Withdraw = Insufficient { have :: Int, need :: Int }

withdraw
  :: TRef Int
  -> Int
  -> RIO () (insufficient :: { have :: Int, need :: Int }) Unit
withdraw account amount = atomically do
  balance <- readTRef account
  when (balance < amount)
    (failSTM (Proxy :: _ "insufficient") { have: balance, need: amount })
  writeTRef account (balance - amount)
```

## `orElse`: try one path, fall back to another

`orElse left right` runs `left`; if `left` retries, it rolls back
`left`'s log and runs `right`. A typed failure in `left` does
*not* fall through:

```purescript
orElse :: forall e a. STM e a -> STM e a -> STM e a
```

The classic use case is "take from queue A, or from queue B, or
block until one of them has something":

```purescript
takeOne queueA `orElse` takeOne queueB
```

`orElse` does not catch typed failures because in practice a
typed failure carries information the fallback needs to handle
explicitly. Falling through on failure silently would lose that
context. Use `catchTag` outside `atomically` if you want
fall-through behaviour on failure.

## Concurrent updates

Because `atomically` is synchronous, two fibers calling
`atomically (modifyTRef counter (_ + 1))` cannot interleave: one
runs to completion, then the other runs. No counter race, no
lost update. This is the property STM is most often reached for,
and it falls out of the JS event-loop model for free.

The fiber boundary matters: across two separate `atomically`
calls a value *can* change. The test suite exercises 50 parallel
increments, with each increment in its own `atomically`, and
confirms the final count is exactly 50.

## When to reach for STM vs. `Ref` vs. `Deferred`

- **`Ref`**: when there's one writer or no concurrency. Trivial,
  cheap. Most state in a typical `RIO` program is fine as a
  `Ref`.
- **`STM`**: when multiple fibers need to read-modify-write
  shared state without races, or when a fiber needs to wait
  until shared state matches a condition (`retry` / `check`).
- **`Deferred`**: when one fiber needs to hand off *one* value
  to another fiber as a write-once cell. Not for ongoing state.

## What `RIO.STM` does not give you yet

- **`TQueue`, `TMap`, `TSemaphore`, `TArray`**. These are
  derivable from `TRef` + the primitives in this module. They
  may land as a follow-up; for now build them in your own
  application code if you need them, or open an issue.
- **Multi-transaction composition.** Each `atomically` is its
  own transaction; there is no way to span one transaction
  across two `atomically` calls.
- **STM-aware `Schedule`**. A schedule cannot ask "did the last
  attempt commit?" Use the action's typed failure / success as
  the schedule's input as usual.
- **Dead-waiter cleanup.** A transaction that retries registers
  callbacks on every read `TRef`. When the transaction is
  killed before a write fires those callbacks, the callbacks
  stay in the waiters list until the next write to that `TRef`
  clears them. This is a small leak under workloads with many
  killed retrying transactions; it does not affect correctness.

## Pointers

- `src/RIO/STM.purs`: the type, primitives, and `atomically`.
- `test/Test/RIO/STMSpec.purs`: tests for commit / abort,
  staged-write visibility, `failSTM` abort + write discard,
  `retry` wakeup, `orElse` fallthrough, and 50-fiber parallel
  increments.
- `docs/06-concurrency.md`: how `interrupt` interacts with a
  suspended `atomically`.
