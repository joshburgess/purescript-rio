-- | `purescript-spec` integration helpers for `RIO`.
-- |
-- | Two small adapters slot an `RIO` program directly into a
-- | `Spec` suite without per-test boilerplate:
-- |
-- |   * `itRIO` runs a fully-handled `RIO () () Unit` program as
-- |     a test body. Defects raised inside the program surface as
-- |     `Aff` exceptions; `Spec` reports them as test failures
-- |     just like any thrown error.
-- |
-- |   * `itRIO_` accepts a record of services and `provideAll`s
-- |     them before running, so service-using programs slot in
-- |     without an explicit `provideAll` at every call site.
-- |
-- | `runSpecRIO` is a one-line convenience that pre-installs the
-- | console reporter and exits the process with the suite's
-- | result.
-- |
-- | Programs with un-handled typed failures should use plain `it`
-- | with `runRIO` and pattern-match on the `Either`; making that
-- | a one-liner would require a `Show (Variant e)` constraint
-- | the caller may not be able to discharge.
module RIO.Spec
  ( itRIO
  , itRIO_
  , runSpecRIO
  ) where

import Prelude

import Effect (Effect)
import Test.Spec (Spec, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import RIO.Core (RIO, provideAll, runRIO')

-- | Run a fully-handled `RIO` program as a spec body. The program's
-- | environment row is empty (`()`) and error row is empty (`()`);
-- | the only way to surface a failure is via a defect (`Effect`
-- | exception, `die`, etc.), which `Spec` treats as a normal test
-- | failure.
-- |
-- | ```purescript
-- | spec = describe "pure laws" do
-- |   itRIO "succeeds on a closed program" do
-- |     x <- pure 42
-- |     liftAff (x `shouldEqual` 42)
-- | ```
itRIO :: String -> RIO () () Unit -> Spec Unit
itRIO name program = it name (runRIO' program)

-- | Run an `RIO` program that requires services, providing them
-- | first via `provideAll`. Use this when several tests share the
-- | same set of mocked services and you do not want the
-- | `provideAll` repeated in every body.
-- |
-- | ```purescript
-- | itRIO_ "logs a greeting" { logger: fakeLogger } do
-- |   info "hello"
-- | ```
itRIO_ :: forall r. String -> Record r -> RIO r () Unit -> Spec Unit
itRIO_ name env program = it name (runRIO' (provideAll env program))

-- | Run a `Spec` suite with the console reporter and exit the
-- | process. This is the same default as `Test.Main` already uses;
-- | reaching for it from a project's `main` saves the two
-- | top-level imports.
-- |
-- | ```purescript
-- | main :: Effect Unit
-- | main = runSpecRIO do
-- |   MySpec.spec
-- | ```
runSpecRIO :: Spec Unit -> Effect Unit
runSpecRIO = runSpecAndExitProcess [ consoleReporter ]
