# Testing RIO Programs

> **Naming convention.** Prose and code samples in this guide
> use the shorthand `RIO.Test`, `RIO.Clock`, `RIO.Test.Clock`,
> and `RIO.Spec`. The concrete modules live under `RIO.Aff.*`
> in the rio-aff package and under `RIO.Fiber.*` in the
> premier rio-fiber package (with the rio-fiber test clock at
> the top-level `RIO.Fiber.TestClock` rather than under a
> `Test/` subdir). The walkthrough applies to both runtimes
> with a mechanical prefix swap.

RIO's testing surface covers:

1. `RIO.Test`: `mockService` and `recording` for replacing one
   service with a stub and capturing call sequences.
2. `RIO.Clock` and `RIO.Test.Clock`: a `Clock` service plus a virtual
   clock you advance manually.
3. `RIO.Spec`: `purescript-spec` integration helpers (`itRIO`,
   `itRIO_`, `runSpecRIO`).
4. Patterns: how to structure tests for layered programs, when to
   reach for which helper.

This doc walks each piece end to end. The examples are drawn from
the test suite at `rio-aff/test/Test/RIO/Aff/` (rio-fiber:
`rio-fiber/test/Test/RIO/Fiber/`).

## Mocking a service

A service in RIO is a record of `Aff` or `Effect`-valued operations
(`docs/02-services.md`). Provide a mock implementation as that same
record, then inject it with `mockService` or `provideAll`.

```purescript
type Notifier = { send :: String -> Aff Unit }

send :: forall r e. String -> RIO (notifier :: Notifier | r) e Unit
send msg = do
  n <- ask (Proxy :: Proxy "notifier")
  liftAff (n.send msg)

program :: forall r e. RIO (notifier :: Notifier | r) e Unit
program = do
  send "started"
  send "midway"
  send "done"
```

A test can supply a no-op notifier directly, or use `recording` to
capture each call for later assertion:

```purescript
it "sends three notifications in order" do
  rec <- recording
  let fake = { send: rec.record }
  _ <- runRIO (provideAll { notifier: fake } program)
  calls <- rec.calls
  calls `shouldEqual` [ "started", "midway", "done" ]
```

`mockService` is an alias for `provide` that reads better in tests:
"the program is run with `notifier` mocked out as `fake`". Use
`provideAll` when the service set is fixed and you can supply the
whole environment at once; use `mockService` when you want one
specific service replaced and the rest threaded through.

## The `Clock` service

`RIO.Clock` defines a service with two operations:

```purescript
type Clock =
  { now   :: Aff Milliseconds
  , sleep :: Milliseconds -> Aff Unit
  }
```

Smart constructors `now` and `sleep` read the record out of the
environment and lift the chosen operation into `RIO`. Production
code uses `liveClock` (`Effect.Now.now` plus `Effect.Aff.delay`);
tests use `newTestClock` from `RIO.Test.Clock`, which returns the
clock to inject plus an `advance` controller:

```purescript
import RIO.Test.Clock (newTestClock)

it "fires only after enough virtual time has passed" do
  tc <- newTestClock
  flag <- liftEffect (Ref.new false)
  let
    program :: RIO (clock :: Clock) () Unit
    program = do
      sleep (Milliseconds 100.0)
      liftEffect (Ref.write true flag)

  _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program))
  Aff.delay (Milliseconds 0.0)

  tc.advance (Milliseconds 50.0)
  Aff.delay (Milliseconds 0.0)
  liftEffect (Ref.read flag) >>= shouldEqual false

  tc.advance (Milliseconds 50.0)
  Aff.delay (Milliseconds 0.0)
  liftEffect (Ref.read flag) >>= shouldEqual true
```

Two things worth pointing out:

- `Aff.delay (Milliseconds 0.0)` after each `advance` yields the
  event loop so the fiber's continuation runs before the test
  checks its observable side effect. Without that yield the
  assertion would race the continuation.
- Multiple pending sleepers fire in deadline order inside a single
  `advance`, so a test that wakes three fibers with one call gets
  a deterministic sequence.

The test clock supports cancellation: if a fiber is killed while
its `sleep` is pending, the canceler removes the sleeper from the
pending list, and a later `advance` does not invoke the dead
fiber's callback.

## `purescript-spec` integration

`RIO.Spec` ships two adapters and a runner wrapper.

```purescript
import RIO.Spec (itRIO, itRIO_, runSpecRIO)

spec :: Spec Unit
spec = describe "Greeter" do
  itRIO "runs a fully-handled program" do
    ref <- liftEffect (Ref.new 0)
    liftEffect (Ref.modify_ (_ + 1) ref)
    n <- liftEffect (Ref.read ref)
    liftAff (n `shouldEqual` 1)

  itRIO_ "provides services up front" { greeter: upperGreeter } do
    msg <- greet "world"
    liftAff (msg `shouldEqual` "HELLO world")
```

`itRIO desc program` runs `program :: RIO () () Unit` via
`runRIO'`. Defects raised inside surface as `Aff` exceptions and
`Spec` reports them as ordinary test failures.

`itRIO_ desc env program` runs `program :: RIO r () Unit` after
`provideAll env program`. Reach for it when a group of tests share
the same service record.

`runSpecRIO` is a one-line convenience: `runSpecAndExitProcess [
consoleReporter ]` with the imports pre-resolved. If your test
suite needs a different reporter, fall back to the long form from
`Test.Spec.Runner.Node`.

If your program still has a typed failure on its row, do not reach
for `itRIO`: surface the `Either` with `runRIO` and pattern-match.
A one-line helper that fails the test on `Left` would need a `Show
(Variant e)` constraint that the caller cannot always discharge.

## Layered programs

For programs built from layers (`docs/04-layers.md`),
the test setup is the same as for any service: build the layer in
the test body, run the program with `provideLayer`, assert on side
effects via `recording` or `Ref`. The end-to-end review fixtures
under `spikes/phase-5-review/` walk one such six-service
application; their assertions are byte-for-byte event logs read
out of a `Ref` that each layer's mock implementation writes to.

A useful structuring tip: keep the layer wiring in a helper at the
top of the test file, then write each test body against the
service surface only. The test file ends up looking the same
whether the layer is mocked or live.

## What's not in the testing surface

- **Property tests via `purescript-quickcheck`.** The law checks
  in the test suite are sampled, not generated. Generators that
  drive `RIO` programs are a future candidate.
- **Snapshot testing.** No built-in support; if you need it, write
  the snapshot to a file from `Aff` and diff in the assertion.
- **Test isolation per fiber.** Per-test isolation is best handled
  by allocating `Ref`s inside each `it` body, which is what the
  examples above already do. `RIO.Local`
  (`docs/11-fiber-local.md`) is available for ambient state with
  scoped overrides if you need it, but reach for it as a feature
  primitive, not a test-isolation primitive.

## Pointers

- `rio-aff/src/RIO/Aff/Clock.purs` and
  `rio-fiber/src/RIO/Fiber/Clock.purs`: the production service.
- `rio-aff/src/RIO/Aff/Test/Clock.purs` and
  `rio-fiber/src/RIO/Fiber/TestClock.purs`: `newTestClock` and
  `advance`.
- `rio-aff/src/RIO/Aff/Spec.purs`: `itRIO`, `itRIO_`,
  `runSpecRIO`.
- `rio-aff/src/RIO/Aff/Test.purs`: `mockService`, `recording`.
- `rio-aff/test/Test/RIO/Aff/ClockSpec.purs`: virtual-clock
  test patterns.
- `rio-aff/test/Test/RIO/Aff/SpecHelpersSpec.purs`: `itRIO` and
  `itRIO_` in use.
- `rio-aff/test/Test/RIO/Aff/TestHelpersSpec.purs`:
  `mockService` and `recording` in use.
