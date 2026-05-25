# Software Transactional Memory

`RIO.Aff.STM` / `RIO.Fiber.STM` is the transactional-state
primitive for `RIO`. An `STM e a` is a pure description of a
transaction; `atomically` runs it as an `RIO` action that
either commits every staged write at once or applies none.

> **Naming convention.** Code samples below use unqualified
> `RIO.STM.*` shorthand and rio-aff names for readability. The
> live imports are `RIO.Aff.STM.*` (rio-aff) or
> `RIO.Fiber.STM.*` (rio-fiber). The names mostly line up, but
> a few categories of rename are worth knowing up front:
>
> - **TQueue.** `newTQueue` (rio-aff, unbounded, STM-valued) is
>   `new n` in rio-fiber (bounded, takes capacity, `Effect`-
>   valued). rio-fiber also exports `newSTM n` for the STM-
>   valued form, and adds `capacityTQueue` / `isFullTQueue` /
>   `tryWriteTQueue` operations that have no rio-aff
>   counterpart.
> - **TMap.** `newTMap` / `insertTMap` / `lookupTMap` /
>   `deleteTMap` / `memberTMap` / `sizeTMap` (rio-aff,
>   STM-valued allocator) are `empty` / `insert` / `lookup` /
>   `delete` / `member` / `size` in rio-fiber, with `empty`
>   being `Effect`-valued rather than `STM`-valued.
> - **TSemaphore.** `newTSemaphore` / `acquireTSemaphore` /
>   `releaseTSemaphore` / `availableTSemaphore` /
>   `withTSemaphore` (rio-aff, STM-valued allocator) are `make`
>   / `acquire` / `release` / `available` / `with` in rio-fiber,
>   with `make` being `Effect`-valued.
> - **TDeferred.** `makeTDeferred` / `succeedTDeferred` /
>   `awaitTDeferred` / `pollTDeferred` (rio-aff, with typed
>   errors via `TDeferred e a`) are `make` / `complete` /
>   `await` / `poll` in rio-fiber (no error row: `TDeferred a`).
>   rio-aff's `failTDeferred` and `tryAwaitTDeferred` do not
>   have rio-fiber counterparts.
> - **Pub/sub.** The primitive is `THub` in rio-aff
>   (`RIO.Aff.STM.THub`, with `newBoundedTHub` /
>   `newSlidingTHub` / `newDroppingTHub` / `newUnboundedTHub`)
>   and `TPubSub` in rio-fiber (`RIO.Fiber.STM.TPubSub`, with a
>   single `make` constructor plus `publish` / `subscribe`
>   operations).
> - **TChan.** `newTChan` (rio-aff) is `new` in rio-fiber; the
>   per-op names (`readTChan`, `writeTChan`, `peekTChan`,
>   `tryReadTChan`, `isEmptyTChan`) match in both packages.
> - **TMVar.** `newTMVar` / `newEmptyTMVar` / `takeTMVar` /
>   `tryTakeTMVar` / `putTMVar` / `tryPutTMVar` / `readTMVar` /
>   `tryReadTMVar` / `isEmptyTMVar` (rio-aff) are `new` /
>   `newEmpty` / `take` / `tryTake` / `put` / `tryPut` / `read` /
>   `tryRead` / `isEmpty` in rio-fiber.
> - **TSet.** `newTSet` / `insertTSet` / `deleteTSet` /
>   `memberTSet` / `sizeTSet` / `nullTSet` / `toArrayTSet`
>   (rio-aff) are `empty` / `insert` / `delete` / `member` /
>   `size` / `null` / `toArray` in rio-fiber, with `empty` being
>   `Effect`-valued.
>
> Where a code sample below uses an rio-aff name, rio-fiber
> readers substitute the matching name from this list.
>
> - **STM error row.** Code samples below use rio-aff's
>   `STM e a` shape (an error row carried by the transaction
>   monad, surfaced via `failSTM`). rio-fiber's `STM` has no
>   error parameter: the type is `newtype STM a`, `failSTM` is
>   not exported, and operations like `orElse` are
>   correspondingly `forall a. STM a -> STM a -> STM a`. To
>   raise a typed failure from a fiber transaction, complete the
>   `atomically` block successfully with an `Either`-like value
>   and surface the error with `fail` (or a `Variant.inj`) in
>   `RIO` once the transaction commits.

The shape mirrors ZIO `STM` / Effect `STM`:

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

The `STM` implementation in `RIO.Aff.STM` / `RIO.Fiber.STM`
exploits that directly. A
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

The `TVar` / `TRef` naming differs between the two packages.
In `RIO.Aff.STM`, `TRef` is the primary newtype and `TVar` is
a type alias for it (`type TVar = TRef`); both names point at
the same value and `newTVar` / `readTVar` / `writeTVar` /
`modifyTVar` exist alongside their `TRef`-suffixed siblings as
muscle-memory aliases. In `RIO.Fiber.STM` the relationship is
reversed: `TVar` is the primary newtype and there is no
`TRef` alias, so the body sections below that talk about
`TRef` (the structural details of cells, the identity story)
should be read as describing rio-fiber's `TVar` under a
different name when consulting the fiber source.

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
takeOne counter = do
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

> **rio-aff only.** This section describes the `failSTM`
> primitive, which exists only in rio-aff (where `STM` carries
> an error row). rio-fiber's `STM a` has no error row and no
> `failSTM`; see the preamble at the top of this doc for the
> recommended rio-fiber pattern.

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
-- rio-fiber: orElse :: forall a. STM a -> STM a -> STM a
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

## Derived structures: `TQueue`, `TMap`, `TSemaphore`, `THub` / `TPubSub`, `TArray`, `TDeferred`, `TChan`, `TMVar`, `TSet`

Nine derived structures ship in submodules under both
`RIO.Aff.STM.*` and `RIO.Fiber.STM.*`. Each is a thin wrapper
around a single `TRef` plus the primitives above; the
implementations are short enough to read in one sitting if you
want to see how they compose. (The pub/sub primitive is named
`THub` on the aff side and `TPubSub` on the fiber side, as
called out in the convention note above.) The first six get
worked examples below; `TChan`, `TMVar`, and `TSet` are
sketched at the end with surface-only summaries.

### `RIO.STM.TQueue`

An unbounded FIFO queue (rio-fiber: bounded, constructed with
`new capacity`; `writeTQueue` retries when the queue is full).
Producers `writeTQueue`; consumers `readTQueue`, which retries
when the queue is empty and wakes up automatically the next
time a producer commits.

```purescript
import RIO.STM (atomically)
import RIO.STM.TQueue (newTQueue, readTQueue, writeTQueue)

example = do
  q <- atomically newTQueue
  _ <- fork (atomically (writeTQueue q 1))
  atomically (readTQueue q)  -- blocks until producer commits
```

Surface: `newTQueue`, `writeTQueue`, `writeAllTQueue`,
`readTQueue`, `tryReadTQueue`, `peekTQueue`, `tryPeekTQueue`,
`flushTQueue`, `isEmptyTQueue`, `lengthTQueue`.

Implementation note: the backing store is a single `Array a`
read via `Array.uncons` and extended via `Array.snoc`. Both are
O(n) on the JS backend; a deque-based replacement can swap in
without changing the public API.

### `RIO.STM.TMap`

A transactional map keyed by an `Ord` type. The headline
combinator is `awaitKey`, which retries until a key is present.

```purescript
import RIO.STM (atomically)
import RIO.STM.TMap (awaitKey, insertTMap, newTMap)

example = do
  m <- atomically newTMap
  _ <- fork (atomically (insertTMap 42 "found-it" m))
  atomically (awaitKey 42 m)  -- blocks until a producer inserts
```

Surface: `newTMap`, `insertTMap`, `lookupTMap`, `deleteTMap`,
`memberTMap`, `sizeTMap`, `keysTMap`, `valuesTMap`,
`entriesTMap`, `clearTMap`, `awaitKey`. Wakeups are not
key-indexed:
any write to the underlying `TRef` re-checks the predicate. This
is fine for typical "wait on handler registration" or "wait on
configuration ready" patterns; if you have a hot map where many
keys are inserted per second and waiters scale with key count,
prefer one `TQueue` per logical channel.

### `RIO.STM.TSemaphore`

A counting semaphore. `acquireN` retries when fewer permits
than requested are available; `releaseN` adds permits back.
`withTSemaphore` brackets a single-permit acquire/release pair
against an `RIO` action so the permit is released on every
termination path.

```purescript
import RIO.STM (atomically)
import RIO.STM.TSemaphore (newTSemaphore, withTSemaphore)

example = do
  sem <- atomically (newTSemaphore 3)
  parTraverse (withTSemaphore sem <<< handle) requests
```

Surface: `newTSemaphore`, `acquireTSemaphore`, `acquireN`,
`tryAcquireTSemaphore`, `tryAcquireN`, `releaseTSemaphore`,
`releaseN`, `availableTSemaphore`, `withTSemaphore`.

Note that `parTraverseN n` already bounds concurrency for the
common case of "run at most n fibers in parallel." Reach for a
`TSemaphore` when the permit needs to span more than one
traversal, when a single fiber needs multiple permits at once,
or when you want to expose the permit pool as a service for
other callers to share.

### `RIO.Aff.STM.THub` (rio-fiber: `RIO.Fiber.STM.TPubSub`)

A transactional publish/subscribe hub. Each published value
fans out to every active subscriber's private buffer;
subscribers consume independently from their own buffers, so a
slow subscriber does not block sibling subscribers (modulo the
chosen back-pressure strategy).

```purescript
import RIO.STM (atomically)
import RIO.STM.THub
  ( newBoundedTHub
  , publishTHub
  , takeSubscription
  , withSubscription
  )

example = do
  hub <- atomically (newBoundedTHub 16)
  withSubscription hub \sub -> do
    _ <- fork (forever (atomically (publishTHub hub "tick")))
    atomically (takeSubscription sub)  -- blocks until first publish
```

Four constructors choose how the hub handles a subscriber whose
buffer is full:

- `newBoundedTHub n`: each subscriber buffer is capped at `n`
  items. `publishTHub` retries (blocks) while any subscriber's
  buffer is full. Producer throughput is dictated by the
  slowest subscriber.
- `newSlidingTHub n`: when a subscriber's buffer is full the
  oldest entry is dropped to make room. `publishTHub` never
  blocks and always returns `true`; the affected subscriber
  loses its oldest pending message.
- `newDroppingTHub n`: when a subscriber's buffer is full the
  *new* message is dropped for that subscriber. `publishTHub`
  never blocks; the return value is `false` if any subscriber
  dropped the message.
- `newUnboundedTHub`: no cap. Producer never blocks and nothing
  is dropped. Susceptible to memory growth if a subscriber
  stops consuming.

`subscribeTHub` registers a fresh subscriber and returns a
`Subscription`. A new subscriber sees only values published
*after* it registers; values that were already in flight are
not retroactively delivered to it. `unsubscribeTHub` removes
the subscription and drops any values still buffered for it.

For most consumer code, prefer `withSubscription`: it brackets
subscribe/unsubscribe against an `RIO` action so the
subscription is released on every termination path.

Surface: `Strategy(..)`, `THub`, `Subscription`, `newTHub`,
`newBoundedTHub`, `newSlidingTHub`, `newDroppingTHub`,
`newUnboundedTHub`, `publishTHub`, `tryPublishTHub`,
`subscribeTHub`, `unsubscribeTHub`, `takeSubscription`,
`tryTakeSubscription`, `isEmptySubscription`,
`lengthSubscription`, `subscriberCount`, `withSubscription`.

Implementation note: each subscriber's buffer is a `TRef (Array
a)`. Publish iterates all current subscribers in one
transaction, so either every subscriber accepts the value or
the whole publish retries (`Bounded`) / records a drop
(`Dropping`). There is no per-subscriber back-pressure on a
shared hub: choose `Bounded` if you want the producer to be
paced by the slowest consumer, `Sliding` / `Dropping` if you
want the producer to outrun a slow consumer at the cost of
losing messages, and `Unbounded` if buffer growth is fine.

### `RIO.STM.TArray`

A fixed-length transactional array. Reads and writes are
indexed and stay atomic at the per-cell granularity.

```purescript
import RIO.STM (atomically)
import RIO.STM.TArray (modifyTArray, newTArray, readTArray)

example = do
  arr <- atomically (newTArray 4 0)
  _ <- atomically (modifyTArray 2 (_ + 1) arr)
  atomically (readTArray 2 arr)  -- Just 1
```

Surface: `newTArray`, `fromArrayTArray`, `replicateTArray`,
`lengthTArray`, `readTArray`, `writeTArray`, `modifyTArray`,
`swapTArray`, `toArrayTArray`. The backing store is a single
`TRef (Array a)`; out-of-range indices return `Nothing` / `false`
rather than retrying.

### `RIO.STM.TDeferred`

The transactional counterpart to `RIO.Aff.Deferred` /
`RIO.Fiber.Deferred`. A `TDeferred`
is a write-once cell whose await composes inside `atomically`
with other STM operations, so you can wait on the cell *and*
drain a queue (or check a flag) in a single transaction.

```purescript
import RIO.STM (atomically)
import RIO.STM.TDeferred (awaitTDeferred, makeTDeferred, succeedTDeferred)

example = do
  ready <- atomically makeTDeferred
  _ <- fork (atomically (succeedTDeferred ready unit))
  atomically (awaitTDeferred ready)  -- retries until succeed
```

Surface: `makeTDeferred`, `succeedTDeferred`, `failTDeferred`,
`awaitTDeferred`, `tryAwaitTDeferred`, `pollTDeferred`. Like
the plain `Deferred`, fills after the first are no-ops; awaits
after the fill see the same value.

### `RIO.STM.TChan`

An unbounded multi-producer / multi-consumer channel. Behaves
like `TQueue` but exposes a separate read and write end so
producers can be dropped without closing the read end, and
multiple consumers can pull from the same channel.

Surface: `newTChan`, `writeTChan`, `readTChan`,
`tryReadTChan`, `peekTChan`, `isEmptyTChan`.

### `RIO.STM.TMVar`

A transactional `MVar`: a single-cell either-empty-or-full
container. `takeTMVar` retries when empty; `putTMVar` retries
when full. Useful as a rendezvous point or a one-element
mailbox where back-pressure should block the producer.

Surface: `newEmptyTMVar`, `newTMVar`, `takeTMVar`, `putTMVar`,
`tryTakeTMVar`, `tryPutTMVar`, `readTMVar`, `tryReadTMVar`,
`isEmptyTMVar`.

### `RIO.STM.TSet`

A transactional set. Like `TMap` but values are unit; the
distinguishing primitive is `memberTSet`. Insertion / deletion
are idempotent at the per-element granularity.

Surface: `newTSet`, `insertTSet`, `deleteTSet`, `memberTSet`,
`sizeTSet`, `nullTSet`, `toArrayTSet`.

## What `RIO.Aff.STM` / `RIO.Fiber.STM` does not give you yet

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

- `rio-aff/src/RIO/Aff/STM.purs` and
  `rio-fiber/src/RIO/Fiber/STM.purs`: the type, primitives, and
  `atomically`.
- `rio-aff/src/RIO/Aff/STM/` and `rio-fiber/src/RIO/Fiber/STM/`:
  the derived structures (`TQueue`, `TMap`, `TSemaphore`,
  `TArray`, `TDeferred`, `TChan`, `TMVar`, `TSet`, plus `THub`
  on aff or `TPubSub` on fiber).
- `rio-aff/test/Test/RIO/Aff/STMSpec.purs`: tests for commit /
  abort, staged-write visibility, `failSTM` abort + write
  discard, `retry` wakeup, `orElse` fallthrough, and 50-fiber
  parallel increments.
- `rio-aff/test/Test/RIO/Aff/STM/` and
  `rio-fiber/test/Test/RIO/Fiber/STM/`: tests for the derived
  structures (FIFO order, blocking dequeue, `awaitKey` wakeup,
  bounded concurrency via `withTSemaphore`).
- `docs/06-concurrency.md`: how `interrupt` interacts with a
  suspended `atomically`.
