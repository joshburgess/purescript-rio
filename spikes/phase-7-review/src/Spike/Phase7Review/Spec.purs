-- | Phase 7 review: spec suite for the layered application.
-- |
-- | Four scenarios drive `appLayer` through the test-helper
-- | surface only:
-- |
-- |   * A. Happy path. `itRIO_` + `recording` for the logger,
-- |     `newTestClock` for the clock. Asserts the recorded log
-- |     lines exactly.
-- |
-- |   * B. Failing layer. `dataLayer` raises `dbConnect` because
-- |     the URL is empty. Uses plain `it` + `runRIO` so the
-- |     `Left` can be pattern-matched.
-- |
-- |   * C. Program failure after service use. The program calls
-- |     `greet 7` then `fail (Proxy :: Proxy "progBoom") unit`.
-- |     Plain `it` + `runRIO`.
-- |
-- |   * D. Time-sensitive behaviour. `greetAfter` sleeps through
-- |     the `Clock`. Two forked operations resume in deadline
-- |     order as virtual time advances; the test interleaves
-- |     `advance` with assertions to drive them.
module Spike.Phase7Review.Spec (spec) where

import Prelude hiding ((>>>))

import Data.Array (range) as Array
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, ask, fail, provideAll, provideLayer, runRIO)
import RIO.Aff.Spec (itRIO_)
import RIO.Aff.Test (recording)
import RIO.Aff.Test.Clock (newTestClock)

import Spike.Phase7Review.Layers (appLayer)

spec :: Spec Unit
spec = describe "Phase 7 review: layered app via test helpers" do
  scenarioA
  scenarioB
  scenarioC
  scenarioD

-- | A: happy path. `itRIO_` provides a `clock` service drawn
-- | from `newTestClock`; the layer is built with a recording
-- | logger. After the program runs, assert the recorded events
-- | byte-for-byte.
scenarioA :: Spec Unit
scenarioA = do
  it "A: happy path - greet runs, logger records every line" do
    rec <- recording
    tc <- newTestClock

    let
      program :: RIO (clock :: Clock) (dbConnect :: String) Unit
      program = provideLayer (appLayer "postgres://demo/db" rec.record) do
        us <- ask (Proxy :: Proxy "userService")
        traverse_ (\uid -> liftAff (us.greet uid)) (Array.range 1 3)
        traverse_ (\uid -> liftAff (us.greet uid)) (Array.range 1 3)

    result <- runRIO (provideAll { clock: tc.clock } program)
    case result of
      Left v ->
        liftAff
          ( shouldEqual
              (showDbConnect v)
              "expected Right"
          )
      Right _ -> pure unit

    events <- rec.calls
    events `shouldEqual`
      [ "db-open url=postgres://demo/db"
      , "greet uid=1"
      , "greet uid=2"
      , "greet uid=3"
      , "greet uid=1"
      , "greet uid=2"
      , "greet uid=3"
      ]

-- | B: failing layer. The `dataLayer` fails before the program
-- | body runs, so only the events emitted before that failure
-- | should appear in the recording.
scenarioB :: Spec Unit
scenarioB = do
  it "B: failing layer - dbConnect surfaces, body skipped" do
    rec <- recording
    tc <- newTestClock

    let
      program :: RIO (clock :: Clock) (dbConnect :: String) Unit
      program = provideLayer (appLayer "" rec.record) do
        _ <- ask (Proxy :: Proxy "userService")
        liftAff (rec.record "should-not-run")

    result <- runRIO (provideAll { clock: tc.clock } program)
    events <- rec.calls

    case result of
      Right _ ->
        liftAff (shouldEqual "expected Left dbConnect" "got Right")
      Left v -> liftAff (showDbConnect v `shouldEqual` "dbConnect: empty database url")

    events `shouldEqual` []

-- | C: program failure after service use. `greet 7` runs once,
-- | then a typed `progBoom` is raised. The recording should show
-- | the single `greet` line and nothing more.
scenarioC :: Spec Unit
scenarioC = do
  it "C: program failure after service use - greet runs, progBoom surfaces" do
    rec <- recording
    tc <- newTestClock

    let
      program
        :: RIO (clock :: Clock) (dbConnect :: String, progBoom :: Unit) Unit
      program = provideLayer (appLayer "postgres://demo/db" rec.record) do
        us <- ask (Proxy :: Proxy "userService")
        _ <- liftAff (us.greet 7)
        fail (Proxy :: Proxy "progBoom") unit

    result <- runRIO (provideAll { clock: tc.clock } program)
    events <- rec.calls

    case result of
      Right _ ->
        liftAff (shouldEqual "expected Left progBoom" "got Right")
      Left v -> liftAff (showBoth v `shouldEqual` "progBoom")

    events `shouldEqual`
      [ "db-open url=postgres://demo/db"
      , "greet uid=7"
      ]

-- | D: time-sensitive behaviour. Two forked `greetAfter` calls
-- | with different deadlines. Neither completes until `advance`
-- | pushes virtual time past the deadline; the test asserts the
-- | recording at each step.
scenarioD :: Spec Unit
scenarioD = do
  itRIO_ "D: greetAfter sleeps until virtual time advances"
    {}
    do
      tc <- liftAff newTestClock
      rec <- liftAff recording
      doneRef <- liftEffect (Ref.new ([] :: Array String))

      let
        record = rec.record

        -- The user service program. Two `greetAfter` calls in
        -- parallel: one at 100ms, one at 200ms.
        spawnUser uid (Milliseconds ms) = liftAff $ Aff.forkAff do
          result <- runRIO
            ( provideAll { clock: tc.clock }
                ( provideLayer (appLayer "postgres://demo/db" record)
                    ( do
                        us <- ask (Proxy :: Proxy "userService")
                        liftAff (us.greetAfter (Milliseconds ms) uid)
                    )
                )
            )
          liftEffect case result of
            Right msg ->
              Ref.modify_ (\xs -> xs <> [ "uid=" <> show uid <> ":" <> msg ])
                doneRef
            Left _ -> pure unit

      _ <- spawnUser 1 (Milliseconds 100.0)
      _ <- spawnUser 2 (Milliseconds 200.0)
      liftAff (Aff.delay (Milliseconds 0.0))

      -- Nothing should have logged or completed yet: both fibers
      -- are parked inside their `clock.sleep`.
      do
        seen <- liftAff rec.calls
        liftAff
          ( seen `shouldEqual`
              [ "db-open url=postgres://demo/db", "db-open url=postgres://demo/db" ]
          )
        done0 <- liftEffect (Ref.read doneRef)
        liftAff (done0 `shouldEqual` [])

      -- Advance halfway: still no greet logs, still no completions.
      liftAff (tc.advance (Milliseconds 50.0))
      liftAff (Aff.delay (Milliseconds 0.0))
      do
        done1 <- liftEffect (Ref.read doneRef)
        liftAff (done1 `shouldEqual` [])

      -- Cross the first deadline: uid=1 wakes and completes.
      liftAff (tc.advance (Milliseconds 50.0))
      liftAff (Aff.delay (Milliseconds 0.0))
      do
        done2 <- liftEffect (Ref.read doneRef)
        liftAff (done2 `shouldEqual` [ "uid=1:hello, alice" ])

      -- Cross the second deadline: uid=2 wakes too.
      liftAff (tc.advance (Milliseconds 100.0))
      liftAff (Aff.delay (Milliseconds 0.0))
      do
        done3 <- liftEffect (Ref.read doneRef)
        liftAff
          ( done3 `shouldEqual`
              [ "uid=1:hello, alice"
              , "uid=2:hello, bob"
              ]
          )

-- | Render only the `dbConnect` tag for assertion strings.
showDbConnect :: Variant.Variant (dbConnect :: String) -> String
showDbConnect = Variant.match
  { dbConnect: \s -> "dbConnect: " <> s }

-- | Render either tag in the C-scenario row.
showBoth :: Variant.Variant (dbConnect :: String, progBoom :: Unit) -> String
showBoth = Variant.match
  { dbConnect: \s -> "dbConnect: " <> s
  , progBoom: \_ -> "progBoom"
  }
