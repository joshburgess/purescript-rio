module Spike.RowInference.Prototype
  ( RIO
  , runRIO
  , ask
  , asks
  , fail
  , provide
  , catchTag
  , liftAff
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Prim.Row (class Cons, class Lacks) as Row
import Record as Record
import Type.Proxy (Proxy)

newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))

unwrap :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
unwrap (RIO f) = f

runRIO :: forall e a. RIO () e a -> Aff (Either (Variant e) a)
runRIO m = unwrap m {}

instance functorRIO :: Functor (RIO r e) where
  map f (RIO g) = RIO \r -> map (map f) (g r)

instance applyRIO :: Apply (RIO r e) where
  apply (RIO f) (RIO g) = RIO \r -> do
    rf <- f r
    case rf of
      Left e -> pure (Left e)
      Right h -> do
        ra <- g r
        pure (map h ra)

instance applicativeRIO :: Applicative (RIO r e) where
  pure a = RIO \_ -> pure (Right a)

instance bindRIO :: Bind (RIO r e) where
  bind (RIO m) k = RIO \r -> do
    res <- m r
    case res of
      Left e -> pure (Left e)
      Right a -> unwrap (k a) r

instance monadRIO :: Monad (RIO r e)

liftAff :: forall r e a. Aff a -> RIO r e a
liftAff a = RIO \_ -> map Right a

ask
  :: forall sym a r' r e
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> RIO r e a
ask sym = RIO \r -> pure (Right (Record.get sym r))

asks
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> (a -> b)
  -> RIO r e b
asks sym f = map f (ask sym)

fail
  :: forall sym a r e' e b
   . IsSymbol sym
  => Row.Cons sym a e' e
  => Proxy sym
  -> a
  -> RIO r e b
fail sym v = RIO \_ -> pure (Left (Variant.inj sym v))

provide
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Row.Lacks sym r'
  => Proxy sym
  -> a
  -> RIO r e b
  -> RIO r' e b
provide sym v (RIO f) = RIO \r' -> f (Record.insert sym v r')

catchTag
  :: forall sym a e' e r b
   . IsSymbol sym
  => Row.Cons sym a e' e
  => Proxy sym
  -> (a -> RIO r e' b)
  -> RIO r e b
  -> RIO r e' b
catchTag sym handler (RIO m) = RIO \r -> do
  res <- m r
  case res of
    Right a -> pure (Right a)
    Left v ->
      Variant.on sym
        (\a -> unwrap (handler a) r)
        (\rest -> pure (Left rest))
        v
