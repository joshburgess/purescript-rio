-- | Route definitions for the todo-api. Uses `Routing.Duplex`
-- | (re-exported by HTTPurple) to derive a typed route from a sum
-- | type. The same value is both a parser and a printer.
module Example.TodoApi.Routes
  ( Route(..)
  , route
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (RouteDuplex', int, mkRoute, noArgs, segment, (/))

-- | Two route shapes:
-- |
-- |   * `Todos`           -> /todos
-- |   * `TodoById id`     -> /todos/:id
-- |
-- | The HTTP method is dispatched on inside the handler; `Routing.Duplex`
-- | itself is method-agnostic.
data Route
  = Todos
  | TodoById Int

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Todos": "todos" / noArgs
  , "TodoById": "todos" / int segment
  }
