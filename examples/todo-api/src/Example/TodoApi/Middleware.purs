-- | App-specific bindings on top of `rio-aff-http`.
-- |
-- | The `RequestContext` type is re-exported verbatim;
-- | `withRequestContext` is re-exported with its env/error rows
-- | pinned to this example's `Env` / `ApiError`. `requireAuth` is a pre-application of
-- | `RIO.Aff.HTTPurple.Auth.requireAuth` that pins the typed-failure
-- | tag to this example's `ApiError` row (`unauthorized :: Unit`)
-- | so call sites can ignore the tag/payload pair.
-- |
-- | A real app would do the same: hold the polymorphic auth
-- | combinator in a sibling helper that chooses its own typed
-- | failure shape.
module Example.TodoApi.Middleware
  ( AuthConfig
  , RequestContext
  , defaultAuthConfig
  , requireAuth
  , withRequestContext
  ) where

import Prelude

import HTTPurple (RequestHeaders)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.HTTPurple.Auth (bearerAuthConfig)
import RIO.Aff.HTTPurple.Auth (AuthConfig, requireAuth) as Auth
import RIO.Aff.HTTPurple.Middleware (withRequestContext) as Reexports
import RIO.Aff.HTTPurple.Request (RequestContext) as Reexports

import Example.TodoApi.Handlers (ApiError, Env)

-- | Re-exported from `RIO.Aff.HTTPurple.Request`.
type RequestContext = Reexports.RequestContext

-- | Re-exported from `RIO.Aff.HTTPurple.Auth`.
type AuthConfig = Auth.AuthConfig

-- | Re-exported from `RIO.Aff.HTTPurple.Middleware`.
withRequestContext
  :: forall a
   . RequestContext
  -> RIO Env ApiError a
  -> RIO Env ApiError a
withRequestContext = Reexports.withRequestContext

-- | This example's bearer-token configuration. Production apps
-- | replace `bearerAuthConfig` with a richer verifier.
defaultAuthConfig :: AuthConfig
defaultAuthConfig = bearerAuthConfig "example-token"

-- | Pre-application of `RIO.Aff.HTTPurple.Auth.requireAuth` against
-- | this example's `unauthorized` typed failure.
requireAuth
  :: AuthConfig
  -> RequestHeaders
  -> RIO Env ApiError Unit
requireAuth cfg hdrs =
  Auth.requireAuth cfg hdrs (Proxy :: Proxy "unauthorized") unit

