module Test.RIO.SqlSpec (spec) where

import Prelude

import Data.Argonaut.Core as Json
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, runRIO)
import RIO.Env (provide)
import RIO.Sql (Sql, SqlError(..), SqlValue(..), Statement, SqlRow, SqlResult)
import RIO.Sql as Sql
import RIO.Schema as Schema

mkSql
  :: { calls :: Ref.Ref (Array Statement)
     , execute :: Statement -> Aff (Either SqlError SqlResult)
     , query :: Statement -> Aff (Either SqlError (Array SqlRow))
     }
  -> Sql
mkSql cfg = Sql.mockSql
  { execute: \s -> do
      liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) cfg.calls)
      cfg.execute s
  , query: \s -> do
      liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) cfg.calls)
      cfg.query s
  }

run
  :: forall a
   . RIO (sql :: Sql) (sqlError :: SqlError) a
  -> Sql
  -> Aff (Either SqlError a)
run p sql = do
  res <- runRIO (provide (Proxy :: Proxy "sql") sql p)
  pure case res of
    Right a -> Right a
    Left v -> Left
      (Variant.case_ # Variant.on (Proxy :: Proxy "sqlError") identity $ v)

okExec :: Int -> Statement -> Aff (Either SqlError SqlResult)
okExec n _ = pure (Right { rowsAffected: n })

emptyRows :: Statement -> Aff (Either SqlError (Array SqlRow))
emptyRows _ = pure (Right [])

spec :: Spec Unit
spec = describe "RIO.Sql (shape-only service)" do
  describe "execute / query" do
    it "passes the statement through to the driver and returns rowsAffected" do
      calls <- liftEffect (Ref.new [])
      let
        sql = mkSql
          { calls, execute: okExec 3, query: emptyRows }
      result <- run
        (Sql.execute (Sql.statement "UPDATE foo SET x = $1" [ SqlInt 1 ]))
        sql
      result `shouldEqual` Right { rowsAffected: 3 }
      recorded <- liftEffect (Ref.read calls)
      case recorded of
        [ s ] -> do
          s.text `shouldEqual` "UPDATE foo SET x = $1"
          s.params `shouldEqual` [ SqlInt 1 ]
        _ -> fail
          ("expected exactly one call, got: " <> show (map _.text recorded))

    it "surfaces driver failures on sqlError" do
      calls <- liftEffect (Ref.new [])
      let
        sql = mkSql
          { calls
          , execute: \_ -> pure (Left (SqlExecutionFailed "syntax error"))
          , query: emptyRows
          }
      result <- run (Sql.execute (Sql.statement "boom" [])) sql
      result `shouldEqual` Left (SqlExecutionFailed "syntax error")

    it "queryOne returns Nothing for an empty result set" do
      calls <- liftEffect (Ref.new [])
      let
        sql = mkSql
          { calls, execute: okExec 0, query: emptyRows }
      result <- run (Sql.queryOne (Sql.statement "select 1" [])) sql
      result `shouldEqual` Right (Nothing :: Maybe SqlRow)

  describe "queryDecode" do
    it "decodes each row through a schema" do
      calls <- liftEffect (Ref.new [])
      let
        rowSchema = Schema.recordOf $
          { id: _, name: _ }
            <$> Schema.field "id" _.id Schema.int
            <*> Schema.field "name" _.name Schema.string
        rows =
          [ Object.fromFoldable
              [ Tuple "id" (SqlInt 1)
              , Tuple "name" (SqlString "ada")
              ]
          , Object.fromFoldable
              [ Tuple "id" (SqlInt 2)
              , Tuple "name" (SqlString "lin")
              ]
          ]
        sql = mkSql
          { calls
          , execute: okExec 0
          , query: \_ -> pure (Right rows)
          }
      result <- run
        (Sql.queryDecode rowSchema (Sql.statement "select id, name from t" []))
        sql
      result `shouldEqual` Right
        [ { id: 1, name: "ada" }
        , { id: 2, name: "lin" }
        ]

    it "surfaces decode failures as SqlDecodeError" do
      calls <- liftEffect (Ref.new [])
      let
        rowSchema = Schema.recordOf $
          { id: _ }
            <$> Schema.field "id" _.id Schema.int
        rows =
          [ Object.fromFoldable
              [ Tuple "id" (SqlString "not-an-int") ]
          ]
        sql = mkSql
          { calls
          , execute: okExec 0
          , query: \_ -> pure (Right rows)
          }
      result <- run
        (Sql.queryDecode rowSchema (Sql.statement "select id from t" []))
        sql
      case result of
        Left (SqlDecodeError _) -> pure unit
        other -> fail ("expected SqlDecodeError, got: " <> show other)

  describe "withTransaction" do
    it "issues BEGIN / body / COMMIT on success" do
      calls <- liftEffect (Ref.new [])
      let
        sql = mkSql
          { calls, execute: okExec 0, query: emptyRows }
      result <- run
        ( Sql.withTransaction do
            _ <- Sql.execute (Sql.statement "INSERT INTO t VALUES (1)" [])
            pure 42
        )
        sql
      result `shouldEqual` Right 42
      recorded <- liftEffect (Ref.read calls)
      map _.text recorded `shouldEqual`
        [ "BEGIN"
        , "INSERT INTO t VALUES (1)"
        , "COMMIT"
        ]

    it "issues BEGIN / body / ROLLBACK on typed failure and re-raises" do
      calls <- liftEffect (Ref.new [])
      let
        sql = mkSql
          { calls
          , execute: \s ->
              if s.text == "boom" then
                pure (Left (SqlExecutionFailed "constraint"))
              else
                pure (Right { rowsAffected: 0 })
          , query: emptyRows
          }
      result <- run
        ( Sql.withTransaction do
            Sql.execute (Sql.statement "boom" [])
        )
        sql
      result `shouldEqual` Left (SqlExecutionFailed "constraint")
      recorded <- liftEffect (Ref.read calls)
      map _.text recorded `shouldEqual`
        [ "BEGIN"
        , "boom"
        , "ROLLBACK"
        ]

  describe "valueToJson" do
    it "reifies each kind to its JSON counterpart" do
      Json.stringify (Sql.valueToJson SqlNull) `shouldEqual` "null"
      Json.stringify (Sql.valueToJson (SqlBoolean true)) `shouldEqual` "true"
      Json.stringify (Sql.valueToJson (SqlInt 7)) `shouldEqual` "7"
      Json.stringify (Sql.valueToJson (SqlString "hi"))
        `shouldEqual` "\"hi\""
