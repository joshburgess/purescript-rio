-- | `Tag` is a tiny wrapper that bundles an environment label
-- | (the row's symbol) with a phantom of the service type it
-- | refers to. It exists for the same reason ZIO and Effect-TS
-- | both ship a `Tag` (or `Context.Tag`): once a project has
-- | many services, repeating `(Proxy :: Proxy "logger")` and
-- | the service type at every call site is noisy, and you want
-- | one definitional source of truth that you import from.
-- |
-- | A `Tag sym a` says "the environment field named `sym`
-- | contains a value of type `a`". Define one per service:
-- |
-- | ```purescript
-- | loggerTag :: Tag "logger" Logger
-- | loggerTag = tag
-- | ```
-- |
-- | Then use the `*T`-suffixed helpers wherever you would
-- | otherwise reach for a `Proxy`:
-- |
-- | ```purescript
-- | -- inferred: RIO (logger :: Logger | r) e Logger
-- | getLogger = askT loggerTag
-- |
-- | runnable = provideT loggerTag myLogger inner
-- | ```
-- |
-- | `Tag` is a `newtype` over `Proxy sym`, so it has zero
-- | runtime cost. The service type `a` is phantom; it only
-- | participates at the type level to constrain what value
-- | each helper produces or consumes.
module RIO.Aff.Tag
  ( Tag(..)
  , tag
  , proxyOf
  , labelOf
  , askT
  , asksT
  , provideT
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))

import RIO.Aff.Env (ask, asks, provide)
import RIO.Aff.Internal (RIO)

-- | A label/service pair. The symbol parameter is the
-- | environment-row field name; the type parameter is the
-- | service stored there.
newtype Tag :: Symbol -> Type -> Type
newtype Tag sym a = Tag (Proxy sym)

-- | Construct a tag. Both parameters are picked up from the
-- | expected type:
-- |
-- | ```purescript
-- | loggerTag :: Tag "logger" Logger
-- | loggerTag = tag
-- | ```
tag :: forall sym a. Tag sym a
tag = Tag (Proxy :: Proxy sym)

-- | Unwrap the underlying `Proxy` so a tag can be passed to
-- | any API that still expects a raw label proxy.
proxyOf :: forall sym a. Tag sym a -> Proxy sym
proxyOf (Tag p) = p

-- | Reflect the tag's label to a `String`. Useful when
-- | logging or building diagnostic messages about which
-- | service is being looked up.
labelOf :: forall sym a. IsSymbol sym => Tag sym a -> String
labelOf _ = reflectSymbol (Proxy :: Proxy sym)

-- | Tag-flavoured `ask`: read the service named by the tag
-- | out of the current environment.
askT
  :: forall sym a r' r e
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Tag sym a
  -> RIO r e a
askT (Tag p) = ask p

-- | Tag-flavoured `asks`: read the service named by the tag
-- | and project a value out of it in one step.
asksT
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Tag sym a
  -> (a -> b)
  -> RIO r e b
asksT (Tag p) f = asks p f

-- | Tag-flavoured `provide`: supply the service named by
-- | the tag, shrinking the required environment row by one
-- | field.
provideT
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Tag sym a
  -> a
  -> RIO r e b
  -> RIO r' e b
provideT (Tag p) v inner = provide p v inner
