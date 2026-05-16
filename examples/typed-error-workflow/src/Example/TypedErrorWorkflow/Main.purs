-- | A worked end-to-end example of the RIO typed-error workflow.
-- |
-- | A small program calls a flaky `userApi` service and is expected
-- | to survive its bad behaviour: transient failures are retried
-- | under an exponential schedule, a circuit breaker fail-fasts after
-- | a streak of failures, and `catchTag` routes each remaining
-- | failure tag to a deliberate outcome (fall back to a cached
-- | value, log and escalate, etc.). Every step runs inside a tracer
-- | span and carries structured log fields so an operator can read
-- | the failure trail off the side.
-- |
-- | The example is fully in-process: the `userApi` is a scripted
-- | mock and the `Logger` and `Tracer` services are recording
-- | backends. After the program runs, the recorded log lines, spans,
-- | and breaker state are inspected; if the observations match the
-- | workflow's invariants the process exits with status 0, otherwise
-- | it exits with status 1 so a CI runner sees the regression.
-- |
-- | Run with:
-- |
-- |   npx spago run -p rio-example-typed-error-workflow
module Example.TypedErrorWorkflow.Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Node.Process (exit')
import Type.Proxy (Proxy(..))

import RIO.CircuitBreaker (CircuitBreaker, Phase(..))
import RIO.CircuitBreaker as CB
import RIO.Clock (Clock, liveClock)
import RIO.Core (RIO, ask, catchAll, catchTag, fail, provideAll, runRIO)
import RIO.Logger (Logger, logError, logInfo, logWarn, withFields)
import RIO.Schedule (exponential, intersect, recurs, retry)
import RIO.Test.Logger (LogRecord, newRecordingLogger)
import RIO.Test.Tracer (newRecordingTracer)
import RIO.Tracer
  ( Span
  , Tracer
  , addAttribute
  , withSpan
  )

-- ---------------------------------------------------------------
-- Environment + error rows
-- ---------------------------------------------------------------

-- | The application's environment row. The `userApi` is the flaky
-- | dependency under test; everything else is the cross-cutting
-- | infrastructure the workflow exercises.
type AppEnv =
  ( logger :: Logger
  , tracer :: Tracer
  , clock :: Clock
  , userApi :: UserApi
  )

-- | The application's domain row before the typed-error handlers
-- | run. `transient` and `permanent` come from the API;
-- | `circuitOpen` is introduced by `CircuitBreaker.withBreaker`
-- | when the breaker is tripped.
type DomainErr =
  ( transient :: String
  , permanent :: String
  , circuitOpen :: Unit
  )

-- ---------------------------------------------------------------
-- The flaky dependency
-- ---------------------------------------------------------------

-- | A scripted user-api outcome.
data Outcome
  = OkOutcome { id :: Int, name :: String }
  | TransientOutcome String
  | PermanentOutcome String

-- | The user-api service: each `fetch` call consumes the next
-- | scripted outcome. If the script is exhausted, calls return a
-- | permanent failure (the API has nothing left to say).
type UserApi =
  { fetch :: Int -> Effect Outcome
  , callCount :: Effect Int
  }

mkUserApi :: Array Outcome -> Effect UserApi
mkUserApi script = do
  scriptRef <- Ref.new script
  countRef <- Ref.new 0
  pure
    { fetch: \_ -> do
        _ <- Ref.modify (_ + 1) countRef
        s <- Ref.read scriptRef
        case Array.uncons s of
          Nothing -> pure (PermanentOutcome "script exhausted")
          Just { head, tail } -> do
            Ref.write tail scriptRef
            pure head
    , callCount: Ref.read countRef
    }

-- | The lowest level of the workflow: read the next scripted
-- | outcome and project it onto a typed-error row. The row is
-- | left open in the remaining tags (`| e`) so the call site can
-- | layer additional failures on top (notably `circuitOpen` from
-- | `CircuitBreaker.withBreaker`).
rawFetch
  :: forall r e
   . Int
  -> RIO (userApi :: UserApi | r)
       (transient :: String, permanent :: String | e)
       { id :: Int, name :: String }
rawFetch _ = do
  api <- ask (Proxy :: Proxy "userApi")
  outcome <- liftEffect (api.fetch 0)
  case outcome of
    OkOutcome user -> pure user
    TransientOutcome msg -> fail (Proxy :: Proxy "transient") msg
    PermanentOutcome msg -> fail (Proxy :: Proxy "permanent") msg

-- ---------------------------------------------------------------
-- The policy
-- ---------------------------------------------------------------

-- | Run the user fetch under the full RIO policy stack, in this
-- | order, top to bottom:
-- |
-- |   1. Tracer `withSpan "user.fetch"` plus a `user.id`
-- |      attribute, and `Logger.withFields [user.id]`, so every
-- |      log line and span emitted inside the bracket carries
-- |      the user identifier.
-- |   2. `CircuitBreaker.withBreaker` is the outermost
-- |      reliability layer: a single logical "fetch this user"
-- |      operation is what the breaker counts, not each
-- |      individual retry attempt. When the breaker is `Open`
-- |      the bracket fail-fasts with `circuitOpen` and the
-- |      retry never gets a chance to run.
-- |   3. Inside the breaker, `Schedule.retry` retries the raw
-- |      fetch under an exponential backoff capped by
-- |      `intersect (recurs 2)`: the initial attempt plus two
-- |      retries. The schedule keeps going on any typed-error
-- |      tag (the `whileInput`-style filtering is omitted for
-- |      brevity), and the final surviving failure rises back
-- |      out through `withBreaker` for accounting.
-- |   4. After the breaker / retry stack returns, three nested
-- |      `catchTag` handlers route each remaining failure tag:
-- |      `transient` and `circuitOpen` fall back to the cached
-- |      user; `permanent` is logged at error and surfaced as
-- |      `Nothing`.
-- |
-- | The function returns a `Maybe`: `Just user` on success,
-- | `Nothing` on the fallback path. The caller folds the
-- | `Nothing` back to a cached value or surfaces it to the user.
fetchUserSafely
  :: forall r
   . CircuitBreaker
  -> Int
  -> RIO
       ( logger :: Logger
       , tracer :: Tracer
       , clock :: Clock
       , userApi :: UserApi
       | r
       )
       ()
       (Maybe { id :: Int, name :: String })
fetchUserSafely breaker userId =
  withFields [ Tuple "user.id" (show userId) ]
    ( withSpan "user.fetch" do
        addAttribute "user.id" (show userId)
        logInfo "starting user fetch"
        handle (attempt breaker userId)
    )
  where
  attempt
    :: CircuitBreaker
    -> Int
    -> RIO
         ( logger :: Logger
         , tracer :: Tracer
         , clock :: Clock
         , userApi :: UserApi
         | r
         )
         DomainErr
         { id :: Int, name :: String }
  attempt b uid =
    CB.withBreaker b
      ( retry
          (intersect (recurs 2) (exponential (Milliseconds 5.0) 2.0))
          (rawFetch uid)
      )

  handle
    :: forall a
     . RIO
         ( logger :: Logger
         , tracer :: Tracer
         , clock :: Clock
         , userApi :: UserApi
         | r
         )
         DomainErr
         a
    -> RIO
         ( logger :: Logger
         , tracer :: Tracer
         , clock :: Clock
         , userApi :: UserApi
         | r
         )
         ()
         (Maybe a)
  handle inner =
    catchTag (Proxy :: Proxy "transient")
      ( \msg -> do
          addAttribute "user.outcome" "transient-exhausted"
          logWarn ("transient failure exhausted retries: " <> msg)
          pure Nothing
      )
      ( catchTag (Proxy :: Proxy "circuitOpen")
          ( \_ -> do
              addAttribute "user.outcome" "circuit-open"
              logWarn "circuit breaker is open; using fallback"
              pure Nothing
          )
          ( catchTag (Proxy :: Proxy "permanent")
              ( \msg -> do
                  addAttribute "user.outcome" "permanent"
                  logError ("permanent failure: " <> msg)
                  pure Nothing
              )
              ( do
                  a <- inner
                  addAttribute "user.outcome" "ok"
                  logInfo "user fetched"
                  pure (Just a)
              )
          )
      )

-- | A defensive top-level catch. Every domain failure has a
-- | dedicated handler in `fetchUserSafely`, so the only way a
-- | failure can reach this boundary is a tag the workflow does not
-- | yet model (introduced later in a refactor). `catchAll` makes
-- | that path visible: it logs the failure verbatim and returns a
-- | `Nothing` rather than crashing the runner.
runOne
  :: forall r
   . CircuitBreaker
  -> Int
  -> RIO
       ( logger :: Logger
       , tracer :: Tracer
       , clock :: Clock
       , userApi :: UserApi
       | r
       )
       ()
       (Maybe { id :: Int, name :: String })
runOne breaker userId =
  catchAll
    ( \v -> do
        logError ("unexpected failure: " <> showVariant v)
        pure Nothing
    )
    (fetchUserSafely breaker userId)
  where
  showVariant :: Variant () -> String
  showVariant = Variant.case_

-- | The whole workflow: spin up a breaker, call three users in
-- | sequence, return the trio of outcomes. Sequencing is
-- | intentional: the per-call retry state is local but the
-- | breaker is shared, so an early failure streak can trip the
-- | breaker for a later call.
program
  :: forall r
   . CircuitBreaker
  -> RIO
       ( logger :: Logger
       , tracer :: Tracer
       , clock :: Clock
       , userApi :: UserApi
       | r
       )
       ()
       (Array (Maybe { id :: Int, name :: String }))
program breaker = do
  r1 <- runOne breaker 1
  r2 <- runOne breaker 2
  r3 <- runOne breaker 3
  pure [ r1, r2, r3 ]

-- ---------------------------------------------------------------
-- Scenario script
-- ---------------------------------------------------------------

-- | The scripted scenario, with `maxFailures = 1` on the breaker.
-- |
-- |   * User 1: a single transient failure, then the success.
-- |     The retry recovers; `withBreaker` sees the success and
-- |     resets the failure counter to 0. Breaker stays `Closed`.
-- |   * User 2: three transient failures. The retry budget
-- |     (`recurs 2` = initial + 2 retries) exhausts, and the
-- |     surviving `transient` failure propagates out through
-- |     `withBreaker`. The breaker counts that one logical
-- |     failure, hits `maxFailures`, and trips to `Open`.
-- |   * User 3: the breaker is `Open`, so `withBreaker`
-- |     fail-fasts with `circuitOpen` before the retry or
-- |     `rawFetch` runs. The handler routes `circuitOpen` to
-- |     the fallback path.
scriptedOutcomes :: Array Outcome
scriptedOutcomes =
  [ TransientOutcome "ENOTFOUND once"
  , OkOutcome { id: 1, name: "alice" }
  , TransientOutcome "ENOTFOUND user-2 attempt-1"
  , TransientOutcome "ENOTFOUND user-2 attempt-2"
  , TransientOutcome "ENOTFOUND user-2 attempt-3"
  -- user-3: breaker is tripped before any of these would matter.
  ]

cachedUser :: { id :: Int, name :: String }
cachedUser = { id: 0, name: "<cached>" }

-- ---------------------------------------------------------------
-- Verification
-- ---------------------------------------------------------------

-- | Per-check result. `Right unit` is a pass; `Left reason` is a
-- | one-line description of the failure.
type Check = Either String Unit

ok :: Check
ok = Right unit

failed :: String -> Check
failed = Left

check :: Boolean -> String -> Check
check b msg = if b then ok else failed msg

-- | The post-run battery of checks.
runChecks
  :: { results :: Array (Maybe { id :: Int, name :: String })
     , callCount :: Int
     , breakerSnap :: CB.Snapshot
     , logs :: Array LogRecord
     , spans :: Array Span
     }
  -> Array (Tuple String Check)
runChecks s =
  [ Tuple "user 1 succeeded after retry"
      ( check
          (Array.index s.results 0 == Just (Just { id: 1, name: "alice" }))
          "expected Just alice for user 1"
      )
  , Tuple "user 2 returned the fallback Nothing -> cached"
      ( check
          (Array.index s.results 1 == Just Nothing)
          "expected Nothing (fallback) for user 2"
      )
  , Tuple "user 3 returned the fallback Nothing -> cached"
      ( check
          (Array.index s.results 2 == Just Nothing)
          "expected Nothing (fallback) for user 3"
      )
  , Tuple "breaker tripped to Open after the user-2 streak"
      ( check (s.breakerSnap.phase == Open)
          ("expected breaker phase Open, got " <> show s.breakerSnap.phase)
      )
  , Tuple "underlying api saw only the calls we expected (5)"
      -- user 1: 2 calls (1 transient, 1 success).
      -- user 2: 3 calls (all transient; retry budget exhausted).
      -- user 3: 0 calls (breaker fail-fasts before rawFetch).
      ( check (s.callCount == 5)
          ("expected exactly 5 underlying api calls, got " <> show s.callCount)
      )
  , Tuple "every user.fetch span carries the user.id attribute"
      ( check
          ( Array.all spanHasUserId
              (Array.filter (\sp -> sp.name == "user.fetch") s.spans)
          )
          "one of the user.fetch spans is missing its user.id attribute"
      )
  , Tuple "every log line carries the user.id field"
      ( check
          ( Array.length s.logs > 0
              && Array.all logHasUserId s.logs
          )
          "expected every log line to be inside the withFields block"
      )
  , Tuple "user 3's outcome was tagged circuit-open"
      ( check
          ( hasOutcomeFor 3 "circuit-open" s.spans
          )
          "expected the user-3 span to carry user.outcome=circuit-open"
      )
  , Tuple "permanent path was not taken in this scenario"
      ( check
          ( not
              ( Array.any
                  (\l -> String.contains (String.Pattern "permanent failure") l.message)
                  s.logs
              )
          )
          "no permanent failure was scripted; the path should be cold"
      )
  ]
  where
  spanHasUserId sp =
    Array.any (\(Tuple k _) -> k == "user.id") sp.attributes
  logHasUserId rec =
    Array.any (\(Tuple k _) -> k == "user.id") rec.fields
  hasOutcomeFor uid outcome spans =
    Array.any
      ( \sp ->
          sp.name == "user.fetch"
            && hasAttr "user.id" (show uid) sp.attributes
            && hasAttr "user.outcome" outcome sp.attributes
      )
      spans
  hasAttr k v =
    Array.any (\(Tuple k' v') -> k == k' && v == v')

-- ---------------------------------------------------------------
-- Pretty printing
-- ---------------------------------------------------------------

renderLog :: LogRecord -> String
renderLog rec =
  "    "
    <> show rec.level
    <> ": "
    <> rec.message
    <> case rec.fields of
      [] -> ""
      fs -> "  " <> String.joinWith ", "
        (map (\(Tuple k v) -> k <> "=" <> v) fs)

renderSpan :: Span -> String
renderSpan sp =
  "    "
    <> sp.name
    <> " ["
    <> show sp.status
    <> "]"
    <> case sp.attributes of
      [] -> ""
      xs -> "  " <> String.joinWith ", "
        (map (\(Tuple k v) -> k <> "=" <> v) xs)

renderCheck :: Tuple String Check -> String
renderCheck (Tuple name result) = case result of
  Right _ -> "  PASS  " <> name
  Left reason -> "  FAIL  " <> name <> "  -- " <> reason

renderResult :: Tuple Int (Maybe { id :: Int, name :: String }) -> String
renderResult (Tuple i r) =
  "  user "
    <> show i
    <> ": "
    <> case r of
      Nothing -> "<fallback to " <> cachedUser.name <> ">"
      Just u -> u.name <> " (id=" <> show u.id <> ")"

-- ---------------------------------------------------------------
-- main
-- ---------------------------------------------------------------

main :: Effect Unit
main = launchAff_ runWorkflow

runWorkflow :: Aff Unit
runWorkflow = do
  -- Allocate the recording mocks and the breaker.
  loggerRec <- newRecordingLogger
  tracerRec <- newRecordingTracer
  apiSvc <- liftEffect (mkUserApi scriptedOutcomes)
  breakerRes <- runRIO
    ( CB.make
        { maxFailures: 1
        , resetTimeout: Milliseconds 60_000.0
        }
    )
  breaker <- case breakerRes of
    Right b -> pure b
    Left _ -> liftEffect do
      Console.log "typed-error-workflow: failed to allocate breaker"
      exit' 1
  let
    env :: Record AppEnv
    env =
      { logger: loggerRec.logger
      , tracer: tracerRec.tracer
      , clock: liveClock
      , userApi: apiSvc
      }
  -- Run the workflow.
  programRes <-
    runRIO (provideAll env (program breaker))
  results <- case programRes of
    Right rs -> pure rs
    Left _ -> liftEffect do
      Console.log "typed-error-workflow: program raised an unhandled failure"
      exit' 1
  -- Drain the recording mocks.
  breakerSnapRes <- runRIO (provideAll env (CB.snapshot breaker))
  breakerSnap <- case breakerSnapRes of
    Right s -> pure s
    Left _ -> liftEffect do
      Console.log "typed-error-workflow: failed to read breaker snapshot"
      exit' 1
  callCount <- liftEffect apiSvc.callCount
  logs <- liftEffect loggerRec.snapshot
  spans <- liftEffect tracerRec.snapshot
  let
    outcomeChecks =
      runChecks
        { results
        , callCount
        , breakerSnap
        , logs
        , spans
        }
    failures =
      Array.filter
        ( \(Tuple _ r) -> case r of
            Right _ -> false
            Left _ -> true
        )
        outcomeChecks
  liftEffect do
    Console.log ""
    Console.log "typed-error-workflow: scenario outcomes"
    Console.log "--------------------------------------"
    for_ (Array.zip (Array.range 1 (Array.length results)) results)
      (\pair -> Console.log (renderResult pair))
    Console.log ""
    Console.log
      ( "breaker snapshot: phase="
          <> show breakerSnap.phase
          <> ", failures="
          <> show breakerSnap.failures
      )
    Console.log ""
    Console.log ("log records (" <> show (Array.length logs) <> "):")
    for_ logs (\r -> Console.log (renderLog r))
    Console.log ""
    Console.log ("tracer spans (" <> show (Array.length spans) <> "):")
    for_ spans (\sp -> Console.log (renderSpan sp))
    Console.log ""
    Console.log "checks:"
    for_ outcomeChecks (\c -> Console.log (renderCheck c))
    Console.log ""
    if Array.null failures then
      Console.log "typed-error-workflow: ok"
    else do
      Console.log
        ( "typed-error-workflow: "
            <> show (Array.length failures)
            <> " check(s) failed"
        )
      exit' 1
