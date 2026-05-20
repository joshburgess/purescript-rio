-- | Runs both qualified-do candidates and prints the results so
-- | the spike is exercised end-to-end on CI.
-- |
-- | Two equivalences are checked:
-- |
-- |   * `Resource.do` vs nested `acquireRelease` produce the same
-- |     events list and the same final value.
-- |
-- |   * `Par.ado` and a sequential `do` produce the same final
-- |     value, but the parallel version finishes in roughly the
-- |     time of the slowest branch (not the sum).
module Spike.QualifiedDo.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.DateTime.Instant (Instant, unInstant)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, provideAll, runRIO)
import RIO.Aff.Resource (acquireRelease)

import Spike.QualifiedDo.Par as Par
import Spike.QualifiedDo.Resource as Resource

type EnvRow =
  ( events :: Ref (Array String)
  )

main :: Effect Unit
main = launchAff_ do
  events <- liftEffect (Ref.new [])

  -- Resource.do path
  liftEffect (Ref.write [] events)
  resDoOut <- runRIO (provideAll { events } resourceQualifiedDo)
  resDoEvents <- liftEffect (Ref.read events)

  -- Nested acquireRelease path
  liftEffect (Ref.write [] events)
  resNestedOut <- runRIO (provideAll { events } resourceNested)
  resNestedEvents <- liftEffect (Ref.read events)

  -- Par.ado path
  parStart <- liftEffect now
  parOut <- runRIO (provideAll { events } parDemo)
  parEnd <- liftEffect now
  let parMs = instantMs parEnd - instantMs parStart

  -- Sequential do path
  seqStart <- liftEffect now
  seqOut <- runRIO (provideAll { events } sequentialDemo)
  seqEnd <- liftEffect now
  let seqMs = instantMs seqEnd - instantMs seqStart

  liftEffect $ log $
    "spike-qualified-do:\n"
      <> "  Resource.do result      = "
      <> showRight resDoOut
      <> "\n"
      <> "  Resource.do events      = "
      <> show resDoEvents
      <> "\n"
      <> "  nested result           = "
      <> showRight resNestedOut
      <> "\n"
      <> "  nested events           = "
      <> show resNestedEvents
      <> "\n"
      <> "  events match            = "
      <> show (resDoEvents == resNestedEvents)
      <> "\n"
      <> "  Par.ado result          = "
      <> showRecord parOut
      <> "\n"
      <> "  sequential do result    = "
      <> showRecord seqOut
      <> "\n"
      <> "  parallel wall-clock ms  = "
      <> show parMs
      <> "\n"
      <> "  sequential wall-clock ms= "
      <> show seqMs

instantMs :: Instant -> Number
instantMs = unwrap <<< unInstant

showRight :: Either (Variant ()) String -> String
showRight = case _ of
  Right s -> s
  Left _ -> "<typed-failure>"

showRecord
  :: Either (Variant ()) { a :: String, b :: String, c :: String }
  -> String
showRecord = case _ of
  Right rec -> "{ a: " <> rec.a <> ", b: " <> rec.b <> ", c: " <> rec.c <> " }"
  Left _ -> "<typed-failure>"

------------------------------------------------------------------
-- Resource.do demonstrations
------------------------------------------------------------------

resourceQualifiedDo :: RIO EnvRow () String
resourceQualifiedDo = Resource.do
  h <- Resource.acquire (record "open:h" *> Resource.pure "H") (\_ -> record "close:h")
  c <- Resource.acquire (record "open:c" *> Resource.pure "C") (\_ -> record "close:c")
  _ <- Resource.liftRIO (record ("use:" <> h <> "+" <> c))
  Resource.pure (h <> "+" <> c)

resourceNested :: RIO EnvRow () String
resourceNested =
  acquireRelease (record "open:h" *> pure "H") (\_ -> record "close:h") \h ->
    acquireRelease (record "open:c" *> pure "C") (\_ -> record "close:c") \c -> do
      _ <- record ("use:" <> h <> "+" <> c)
      pure (h <> "+" <> c)

record :: forall e. String -> RIO EnvRow e Unit
record tag = do
  evRef <- ask (Proxy :: Proxy "events")
  liftAff (liftEffect (Ref.modify_ (\xs -> xs <> [ tag ]) evRef))

------------------------------------------------------------------
-- Par.ado demonstrations
------------------------------------------------------------------

slow :: Number -> String -> RIO EnvRow () String
slow ms label = do
  liftAff (Aff.delay (Milliseconds ms))
  pure label

parDemo :: RIO EnvRow () { a :: String, b :: String, c :: String }
parDemo = Par.ado
  a <- slow 100.0 "A"
  b <- slow 100.0 "B"
  c <- slow 100.0 "C"
  in { a, b, c }

sequentialDemo :: RIO EnvRow () { a :: String, b :: String, c :: String }
sequentialDemo = do
  a <- slow 100.0 "A"
  b <- slow 100.0 "B"
  c <- slow 100.0 "C"
  pure { a, b, c }
