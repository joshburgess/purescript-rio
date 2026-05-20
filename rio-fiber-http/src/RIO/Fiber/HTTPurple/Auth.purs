-- | A minimal bearer-token check expressed as a `RIO`
-- | combinator. If the inbound `Authorization` header matches
-- | `cfg.expected`, the action falls through; otherwise the
-- | caller-supplied typed failure is raised on the error row.
-- |
-- | The configuration shape is intentionally small (`AuthConfig`
-- | is a single field) so production deployments can swap in a
-- | richer verifier (JWT, session-store lookup, mTLS) by
-- | calling `fail` with the same tag from their own code; this
-- | module is just the simplest case.
module RIO.Fiber.HTTPurple.Auth
  ( AuthConfig
  , bearerAuthConfig
  , requireAuth
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant as Variant
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

import HTTPurple (RequestHeaders, lookup)

import RIO.Fiber.Core (RIO, fail)

-- | Auth configuration. `expected` is the string the inbound
-- | `Authorization` header must equal exactly. For a bearer
-- | token of `xyz`, `expected` is `"Bearer xyz"`.
type AuthConfig =
  { expected :: String
  }

-- | Convenience constructor: builds an `AuthConfig` whose
-- | `expected` is `"Bearer " <> token`.
bearerAuthConfig :: String -> AuthConfig
bearerAuthConfig token = { expected: "Bearer " <> token }

-- | Check the `Authorization` header against `cfg.expected`.
-- | Match: return `unit`. Missing or wrong: raise the named
-- | typed failure on the error row with the supplied payload.
-- |
-- | The failure tag and payload are supplied by the caller so
-- | the same `requireAuth` can be reused across handlers that
-- | each thread their own `unauthorized` (or `forbidden`, or
-- | `authRequired`, ...) tag through the error row.
requireAuth
  :: forall sym a r e e'
   . IsSymbol sym
  => Cons sym a e' e
  => AuthConfig
  -> RequestHeaders
  -> Proxy sym
  -> a
  -> RIO r e Unit
requireAuth cfg hdrs tag payload =
  case lookup hdrs "Authorization" of
    Just got | got == cfg.expected -> pure unit
    _ -> fail (Variant.inj tag payload)
