-- | End-to-end demo of the row-typed style.
-- |
-- | A small "batch import" pipeline that exercises every error
-- | combinator added in the most recent round of work:
-- |
-- |   * `memoize` for a one-shot config load shared by every step
-- |   * `validatePar` to accumulate every parse error, rather than
-- |     short-circuiting on the first
-- |   * `orElse` for primary / backup lookup
-- |   * `option` to soft-degrade a non-critical enrichment step
-- |   * `foldRIO` to branch the per-record outcome into a unified
-- |     report
-- |
-- | The whole thing runs on row-typed services (a config and a
-- | console) so the same program could be re-run under different
-- | configs or against a captured logger by swapping the env
-- | record passed to `provideAll`.
-- |
-- | Run it with:
-- |
-- | ```
-- | npx spago run -p rio-example-batch-import
-- | ```
module Example.BatchImport.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Traversable (traverse_)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Type.Proxy (Proxy(..))

import RIO.Console as Console
import RIO.Core
  ( RIO
  , ask
  , fail
  , foldRIO
  , mapError
  , memoize
  , option
  , orElse
  , provideAll
  , runRIO
  , validatePar
  )

-- | The environment record. Services live in a row by name; the
-- | service implementations are plain records (`Config`) or
-- | typeclass-free interfaces. No global state, no IO trickery,
-- | just two named fields a caller plugs in at the entry point.
type Env =
  ( config :: Config
  )

-- | Configuration loaded once at startup and shared by every
-- | request. Memoized so a real load (a file read, a remote
-- | lookup, etc.) would run exactly once even if many subsystems
-- | call `getConfig`.
type Config =
  { primarySource :: String
  , backupSource :: String
  , enrichmentEnabled :: Boolean
  }

-- | The error row for the parse step.
type ParseError = (parse :: String)

-- | The error row for the lookup step. The two tags model the two
-- | sources that can fail independently.
type LookupError =
  ( primaryDown :: String
  , notFound :: String
  )

type UserId = Int

-- A successful import. The `enrichment` field is `Nothing` when
-- the optional enrichment step failed and was soft-handled with
-- `option` rather than escalated to a typed failure.
type ImportedUser =
  { id :: UserId
  , source :: String
  , enrichment :: Maybe String
  }

main :: Effect Unit
main = launchAff_ do
  let
    config :: Config
    config =
      { primarySource: "users-primary-db"
      , backupSource: "users-backup-db"
      , enrichmentEnabled: true
      }
  _ <- runRIO (provideAll { config } program)
  pure unit

-- | The end-to-end pipeline. Note the error row is `()` because
-- | every typed failure is handled inside the program: `validatePar`
-- | reflects parse errors into the success channel, and `foldRIO`
-- | unifies the per-record success / failure cases into a single
-- | reporting step.
program :: RIO Env () Unit
program = do
  -- One-shot config load. Even if many subsystems below called
  -- `getConfig` concurrently, the underlying action would run
  -- exactly once thanks to `memoize`'s single-flight semantics.
  getConfig <- memoize loadConfig

  Console.log "[batch-import] start"

  cfg <- getConfig
  Console.log ("[config] primary=" <> cfg.primarySource)

  -- Pass 1: a mixed input set that demonstrates `validatePar`'s
  -- error-accumulation semantics. Every parser runs in parallel and
  -- every failure is reported, rather than short-circuiting on the
  -- first one.
  Console.log "[pass 1] validatePar on mixed input"
  let
    mixedRaw :: Array String
    mixedRaw =
      [ "1", "2", "not-a-number", "3", "", "4" ]

  mixed <- validatePar parseRequest mixedRaw
  case mixed of
    Left errs -> reportParseErrors errs
    Right requests ->
      Console.log
        ( "[parse] all " <> show (Array.length requests) <> " inputs parsed"
        )

  -- Pass 2: an all-valid input set so we can drive the per-record
  -- pipeline. Each request runs `importOne`, which combines `orElse`
  -- (primary / backup lookup) with `option` (best-effort enrichment),
  -- and `foldRIO` unifies the success and typed-failure arms into a
  -- single "report this outcome" step.
  Console.log "[pass 2] per-record pipeline on clean input"
  let
    cleanRaw :: Array String
    cleanRaw = [ "1", "2", "3", "4", "5" ]

  clean <- validatePar parseRequest cleanRaw
  case clean of
    Left errs -> reportParseErrors errs
    Right requests -> traverse_ (handleOne getConfig) requests

  Console.log "[batch-import] done"

handleOne
  :: RIO Env () Config
  -> UserId
  -> RIO Env () Unit
handleOne getConfig uid =
  foldRIO
    (reportLookupFailure uid)
    (reportImported)
    (importOne getConfig uid)

importOne
  :: RIO Env () Config
  -> UserId
  -> RIO Env LookupError ImportedUser
importOne getConfig uid = do
  -- The memoized config accessor has the empty error row `()`, so we
  -- widen it into the per-record `LookupError` row using `mapError`.
  -- `Variant.case_` is the absurd-on-empty-row eliminator, so the
  -- widening is total.
  cfg <- mapError Variant.case_ getConfig
  -- Try the primary source; on typed failure, fall back to the
  -- backup with `orElse`. The two branches have the same error
  -- row, so this is a straight retry-on-failure.
  located <- orElse
    (lookupPrimary cfg uid)
    (lookupBackup cfg uid)

  -- Enrichment is "best-effort": we don't want a failure in the
  -- enrichment service to fail the whole import. `option` reflects
  -- a typed failure into a `Maybe`, so we just record "we tried
  -- but couldn't enrich" and move on.
  enrichment <-
    if cfg.enrichmentEnabled then option (enrich uid)
    else pure Nothing

  pure { id: uid, source: located, enrichment }

-- | A typed lookup that fails on a hard-coded "bad" id. In a real
-- | program this would read from the row-typed service.
lookupPrimary :: Config -> UserId -> RIO Env LookupError String
lookupPrimary cfg uid
  | uid == 3 = fail (Proxy :: Proxy "primaryDown") ("uid " <> show uid)
  | uid == 4 = fail (Proxy :: Proxy "notFound") ("uid " <> show uid)
  | otherwise = pure cfg.primarySource

lookupBackup :: Config -> UserId -> RIO Env LookupError String
lookupBackup cfg uid
  | uid == 4 = fail (Proxy :: Proxy "notFound") ("uid " <> show uid)
  | otherwise = pure cfg.backupSource

-- | An optional enrichment step that fails on one specific id to
-- | exercise the `option` soft-handling path. The error row is a
-- | fresh "enrich" tag; the caller doesn't have to know what tags
-- | this raises because `option` discharges the entire row.
enrich :: UserId -> RIO Env (enrich :: String) String
enrich uid
  | uid == 2 = fail (Proxy :: Proxy "enrich") "rate-limited"
  | otherwise = pure ("info for user " <> show uid)

-- | Parse one line of input. Empty lines and non-numeric lines are
-- | typed failures on the `parse` tag.
parseRequest :: String -> RIO Env ParseError UserId
parseRequest s =
  case Int.fromString (trim s) of
    Nothing -> fail (Proxy :: Proxy "parse") ("cannot parse: " <> show s)
    Just n -> pure n

-- | A pretend "load config" effect. In a real program this would
-- | read from disk or a remote endpoint; here it just returns the
-- | config from the env and logs that it ran (so the demo can show
-- | the memoize cache hit).
loadConfig :: RIO Env () Config
loadConfig = do
  Console.log "[config] loading (this should only print once)"
  -- Read the same record `provideAll` installed at the entry
  -- point. In a fuller demo this would be a slow I/O call.
  ask (Proxy :: Proxy "config")

reportParseErrors
  :: NonEmptyArray (Variant ParseError)
  -> RIO Env () Unit
reportParseErrors errs = do
  Console.error
    ( "[parse] " <> show (NEA.length errs) <> " input(s) failed to parse:"
    )
  traverse_
    ( \v ->
        let
          msg =
            Variant.case_
              # Variant.on (Proxy :: Proxy "parse") identity
              $ v
        in
          Console.error ("  - " <> msg)
    )
    (NEA.toArray errs)

reportLookupFailure :: UserId -> Variant LookupError -> RIO Env () Unit
reportLookupFailure uid v =
  Console.warn
    ( "[lookup] uid "
        <> show uid
        <> " failed: "
        <> renderLookupFailure v
    )

renderLookupFailure :: Variant LookupError -> String
renderLookupFailure =
  Variant.case_
    # Variant.on (Proxy :: Proxy "primaryDown")
        (\msg -> "primary down (" <> msg <> ")")
    # Variant.on (Proxy :: Proxy "notFound")
        (\msg -> "not found (" <> msg <> ")")

reportImported :: ImportedUser -> RIO Env () Unit
reportImported u =
  Console.log
    ( "[ok] uid="
        <> show u.id
        <> " from="
        <> u.source
        <> case u.enrichment of
          Just e -> " enrichment=" <> e
          Nothing -> " enrichment=skipped"
    )
