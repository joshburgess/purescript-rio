-- | Basic testing helpers for `RIO`: `mockService` for swapping a
-- | service record into the environment, and `recording` for
-- | capturing every call to a function-shaped service.
-- |
-- | The rest of the testing surface lives next to its subject:
-- | `RIO.Fiber.Test.Clock` (`TestClock`), `RIO.Fiber.Test.Random`,
-- | `RIO.Fiber.Test.Property` (`forAllRIO`), and `RIO.Fiber.Spec`
-- | (`itRIO` and the `purescript-spec` integration).
module RIO.Fiber.Test
  ( mockService
  , Recording
  , recording
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Ref as Ref
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import RIO.Fiber.Core (RIO)
import RIO.Fiber.Env (provideAt)

-- | An alias for `provideAt` that reads better in tests: "the
-- | program is run with `sym` mocked out as `impl`".
-- |
-- | ```purescript
-- | result <- runRIO
-- |   $ mockService (Proxy :: Proxy "logger") fakeLogger
-- |   $ program
-- | ```
mockService
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> a
  -> RIO r e b
  -> RIO r' e b
mockService = provideAt

-- | A pair of bound operations used to record service calls in tests:
-- | embed `record` in the mock implementation, assert on `calls` after
-- | the program has run.
-- |
-- | Both operations are `Effect`-valued so they slot directly into
-- | service records (whose operations are `Effect`-valued in
-- | rio-fiber, where the runtime drives `Effect` directly).
type Recording a =
  { record :: a -> Effect Unit
  , calls :: Effect (Array a)
  }

-- | Allocate a fresh `Recording`. Use the resulting `record` field as
-- | (part of) a service implementation, then read `calls` after running
-- | the program to assert that the right operations happened in the
-- | right order.
-- |
-- | ```purescript
-- | rec <- recording
-- | let fakeLogger = { log: \_ msg -> rec.record msg }
-- | _ <- runRIO (provideAll { logger: fakeLogger } program)
-- | calls <- rec.calls
-- | calls `shouldEqual` ["starting", "ok"]
-- | ```
recording :: forall a. Effect (Recording a)
recording = do
  ref <- Ref.new []
  pure
    { record: \x -> Ref.modify_ (\xs -> xs <> [ x ]) ref
    , calls: Ref.read ref
    }
