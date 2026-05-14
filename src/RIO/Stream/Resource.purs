-- | Resource-safe stream constructors.
-- |
-- | The base `RIO.Stream` is environment-polymorphic and does not
-- | itself know about `Scope`. That keeps the simple combinators
-- | (`map`, `filter`, `concat`, ...) free of row-constraint noise.
-- |
-- | Once you reach for a stream backed by an OS resource (a file
-- | handle, a Postgres cursor, a network socket), the resource's
-- | lifetime has to be tied to something. This module pins it to
-- | the enclosing `Scope`: the stream's resource is released when
-- | the surrounding `scoped` block exits, on every termination path
-- | (success, typed failure, defect, or fiber kill).
-- |
-- | The scope-as-lifetime model is the same one ZIO uses (`ZStream`
-- | requires `Scope` in its environment when the stream owns
-- | resources).
-- |
-- | ```purescript
-- | -- open a file, stream its lines, guarantee the handle closes
-- | -- when the surrounding `scoped` block exits
-- | program = scoped do
-- |   linesOut <- runCollect
-- |     ( flatMap
-- |         (bracketStream openFile closeFile)
-- |         (\handle -> linesFrom handle)
-- |     )
-- |   pure linesOut
-- | ```
module RIO.Stream.Resource
  ( bracketStream
  ) where

import Prelude

import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO)
import RIO.Env (ask)
import RIO.Resource (Scope, addFinalizer)
import RIO.Stream (Step(..), Stream(..), empty)

-- | A single-element stream that acquires a resource and registers
-- | its release with the enclosing scope. The resource is released
-- | when the scope exits, not when the stream itself completes.
-- |
-- | Compose with `flatMap` to build a multi-element stream that
-- | uses the acquired resource across many yields.
-- |
-- | If `acquire` fails (typed or defect), the release action is not
-- | registered (there is nothing to release) and the failure
-- | propagates unchanged. If the consumer drains only part of the
-- | resulting stream, the resource still releases when the scope
-- | exits.
bracketStream
  :: forall r e a
   . RIO (scope :: Scope | r) e a
  -> (a -> Aff Unit)
  -> Stream (scope :: Scope | r) e a
bracketStream acquire release = Stream do
  scope <- ask (Proxy :: Proxy "scope")
  a <- acquire
  addFinalizer scope (release a)
  pure (Yield a empty)
