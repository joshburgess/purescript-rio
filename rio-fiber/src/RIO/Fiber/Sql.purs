-- | A shape-only SQL service. Defines a small backend-agnostic
-- | surface (`Sql` service record, `Statement`, `SqlValue`, `SqlRow`,
-- | `SqlError`) so application code can be written against one API
-- | regardless of which driver is wired in. A reusable
-- | `mockSql` is provided for tests.
-- |
-- | The service is intentionally minimal: parameterised
-- | execution, row-returning queries, and transactional bracketing.
-- | Anything richer (prepared statements, streaming results,
-- | listen/notify) belongs in a driver-specific extension module
-- | that depends on this one.
-- |
-- | Result rows are returned as `Object SqlValue` so an
-- | application can layer a `RIO.Fiber.Schema` decoder on top with
-- | `queryDecode`. `SqlValue` covers the small set of types that
-- | every SQL driver agrees on (`SqlNull`, `SqlBoolean`, `SqlInt`,
-- | `SqlNumber`, `SqlString`, `SqlJson`); driver-specific kinds
-- | (UUID, interval, bytea, ...) ride inside `SqlJson` or
-- | `SqlString` until a richer driver promotes them.
-- |
-- | Transactions are implemented at the RIO layer (`BEGIN` /
-- | `COMMIT` / `ROLLBACK` issued through `execute`) so they work
-- | against any driver without needing a separate
-- | `withTransaction` hook on the service record. Rollback runs
-- | on success, typed failure, and defect paths alike.
module RIO.Fiber.Sql
  ( Sql
  , Statement
  , SqlValue(..)
  , SqlRow
  , SqlResult
  , SqlError(..)
  , statement
  , execute
  , query
  , queryOne
  , queryDecode
  , withTransaction
  , mockSql
  , describeValue
  , valueToJson
  , rowToJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))

import RIO.Fiber.Aff (fromAff)
import RIO.Fiber.Core (RIO, catchAll, fail)
import RIO.Fiber.Env (askAt)
import RIO.Fiber.Schema (Schema)
import RIO.Fiber.Schema as Schema

-- | The SQL service. A backend is two `Aff` computations:
-- | parameterised execution (no rows back) and parameterised
-- | query (rows back as `Object SqlValue`). Transactional
-- | bracketing is layered on top by `withTransaction`, which
-- | issues `BEGIN` / `COMMIT` / `ROLLBACK` through `execute`.
type Sql =
  { execute :: Statement -> Aff (Either SqlError SqlResult)
  , query :: Statement -> Aff (Either SqlError (Array SqlRow))
  }

-- | A parameterised SQL statement. The text uses driver-native
-- | placeholders (Postgres `$1, $2`, MySQL `?`); this module does
-- | not rewrite them.
type Statement =
  { text :: String
  , params :: Array SqlValue
  }

-- | A SQL value, as wide as the intersection of common drivers.
-- | Driver-specific kinds (UUID, interval, bytea, ...) ride inside
-- | `SqlJson` or `SqlString` until a driver-specific wrapper
-- | promotes them to first-class.
data SqlValue
  = SqlNull
  | SqlBoolean Boolean
  | SqlInt Int
  | SqlNumber Number
  | SqlString String
  | SqlJson Json

derive instance eqSqlValue :: Eq SqlValue

instance showSqlValue :: Show SqlValue where
  show = case _ of
    SqlNull -> "SqlNull"
    SqlBoolean b -> "(SqlBoolean " <> show b <> ")"
    SqlInt i -> "(SqlInt " <> show i <> ")"
    SqlNumber n -> "(SqlNumber " <> show n <> ")"
    SqlString s -> "(SqlString " <> show s <> ")"
    SqlJson _ -> "(SqlJson _)"

-- | Describe a `SqlValue` by its kind tag. Useful for diagnostics
-- | and for `SqlError` rendering.
describeValue :: SqlValue -> String
describeValue = case _ of
  SqlNull -> "null"
  SqlBoolean _ -> "boolean"
  SqlInt _ -> "int"
  SqlNumber _ -> "number"
  SqlString _ -> "string"
  SqlJson _ -> "json"

-- | Reify a `SqlValue` as `Json`. `SqlNull` becomes JSON null;
-- | `SqlBoolean` / `SqlInt` / `SqlNumber` / `SqlString` become
-- | their JSON counterparts; `SqlJson` passes through.
valueToJson :: SqlValue -> Json
valueToJson = case _ of
  SqlNull -> Json.jsonNull
  SqlBoolean b -> Json.fromBoolean b
  SqlInt i -> Json.fromNumber (Int.toNumber i)
  SqlNumber n -> Json.fromNumber n
  SqlString s -> Json.fromString s
  SqlJson j -> j

-- | Reify a `SqlRow` as `Json`, column-by-column.
rowToJson :: SqlRow -> Json
rowToJson row =
  Json.fromObject (Object.mapWithKey (\_ v -> valueToJson v) row)

-- | A row returned from `query`. Keyed by column name.
type SqlRow = Object SqlValue

-- | The result of a non-row-returning `execute`. Currently only
-- | reports `rowsAffected`; extend in a backwards-compatible way
-- | when a driver surfaces more.
type SqlResult =
  { rowsAffected :: Int
  }

-- | A reason a SQL operation failed. Drivers map their native
-- | error shapes onto these tags; preserving the original message
-- | as a `String` keeps the surface stable while still letting
-- | callers log the underlying detail.
data SqlError
  = SqlConnectionFailed String
  | SqlExecutionFailed String
  | SqlIntegrityViolation String
  | SqlTimeout
  | SqlDecodeError String

derive instance eqSqlError :: Eq SqlError

instance showSqlError :: Show SqlError where
  show = case _ of
    SqlConnectionFailed s -> "(SqlConnectionFailed " <> show s <> ")"
    SqlExecutionFailed s -> "(SqlExecutionFailed " <> show s <> ")"
    SqlIntegrityViolation s -> "(SqlIntegrityViolation " <> show s <> ")"
    SqlTimeout -> "SqlTimeout"
    SqlDecodeError s -> "(SqlDecodeError " <> show s <> ")"

-- | Build a `Statement` from text and a parameter list.
statement :: String -> Array SqlValue -> Statement
statement text params = { text, params }

-- | Execute a statement that does not return rows. Returns the
-- | driver's `rowsAffected`. Surfaces driver failures on the
-- | `sqlError` row tag.
execute
  :: forall r e
   . Statement
  -> RIO (sql :: Sql | r) (sqlError :: SqlError | e) SqlResult
execute stmt = do
  s <- askAt (Proxy :: Proxy "sql")
  result <- fromAff (s.execute stmt)
  case result of
    Left err -> fail (Variant.inj (Proxy :: Proxy "sqlError") err)
    Right ok -> pure ok

-- | Run a query and return every row.
query
  :: forall r e
   . Statement
  -> RIO (sql :: Sql | r) (sqlError :: SqlError | e) (Array SqlRow)
query stmt = do
  s <- askAt (Proxy :: Proxy "sql")
  result <- fromAff (s.query stmt)
  case result of
    Left err -> fail (Variant.inj (Proxy :: Proxy "sqlError") err)
    Right rows -> pure rows

-- | Run a query and return the first row, if any. Discards
-- | subsequent rows even if the driver yields more than one.
queryOne
  :: forall r e
   . Statement
  -> RIO (sql :: Sql | r) (sqlError :: SqlError | e) (Maybe SqlRow)
queryOne stmt = Array.head <$> query stmt

-- | Run a query and decode each row through a `Schema`. Surfaces
-- | decode failures on the `sqlError` row tag as `SqlDecodeError`.
-- |
-- | SqlRows are reified to JSON objects (`valueToJson` per column)
-- | before being handed to the schema. SqlRows that already arrive
-- | as a single `SqlJson` blob should be decoded by hand from
-- | `query`.
queryDecode
  :: forall r e a
   . Schema a
  -> Statement
  -> RIO (sql :: Sql | r) (sqlError :: SqlError | e) (Array a)
queryDecode schema stmt = do
  rows <- query stmt
  case traverse (Schema.decode schema <<< rowToJson) rows of
    Left e -> fail
      ( Variant.inj (Proxy :: Proxy "sqlError")
          (SqlDecodeError (Schema.renderError e))
      )
    Right xs -> pure xs

-- | Bracket an action in a database transaction. Issues `BEGIN`
-- | before the body, `COMMIT` on success, and `ROLLBACK` on any
-- | typed failure. The original failure (or success value) is
-- | propagated unchanged once the bracket is closed.
-- |
-- | Defects and external kills also unwind the transaction,
-- | though `ROLLBACK` may not run on those paths (the driver is
-- | expected to roll back any unterminated transaction when the
-- | connection is released or replaced).
withTransaction
  :: forall r e a
   . RIO (sql :: Sql | r) (sqlError :: SqlError | e) a
  -> RIO (sql :: Sql | r) (sqlError :: SqlError | e) a
withTransaction body = do
  _ <- execute (statement "BEGIN" [])
  catchAll
    ( \v -> do
        _ <- execute (statement "ROLLBACK" [])
        fail v
    )
    ( do
        a <- body
        _ <- execute (statement "COMMIT" [])
        pure a
    )

-- | A mock SQL backend for tests. Supply a pair of handlers for
-- | `execute` and `query`; `withTransaction` will issue `BEGIN` /
-- | `COMMIT` / `ROLLBACK` against the supplied `execute` handler.
-- |
-- | Tests that need rollback semantics should layer a `Ref`-based
-- | snapshot over the mock and inspect the issued statements
-- | through the `execute` handler.
mockSql
  :: { execute :: Statement -> Aff (Either SqlError SqlResult)
     , query :: Statement -> Aff (Either SqlError (Array SqlRow))
     }
  -> Sql
mockSql cfg =
  { execute: cfg.execute
  , query: cfg.query
  }
