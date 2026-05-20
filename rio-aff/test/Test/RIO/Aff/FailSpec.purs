module Test.RIO.Aff.FailSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, runRIO)
import RIO.Aff.Error (catchTag)
import RIO.Aff.Fail (class FailWith, failWith)

-- Two domain error types with FailWith instances. Each binds to its
-- own row tag; the instance dispatch is what differentiates them
-- without proxies at the call site.
data DatabaseError = ConnectionLost | QueryFailed String

instance Eq DatabaseError where
  eq ConnectionLost ConnectionLost = true
  eq (QueryFailed a) (QueryFailed b) = a == b
  eq _ _ = false

instance Show DatabaseError where
  show ConnectionLost = "ConnectionLost"
  show (QueryFailed q) = "QueryFailed " <> q

instance FailWith DatabaseError "database"

data ValidationError = MissingField String

instance Eq ValidationError where
  eq (MissingField a) (MissingField b) = a == b

instance Show ValidationError where
  show (MissingField f) = "MissingField " <> f

instance FailWith ValidationError "validation"

spec :: Spec Unit
spec = describe "RIO.Aff.Fail" do

  it "failWith routes a domain error to its declared tag" do
    let
      program :: RIO () (database :: DatabaseError) Int
      program = failWith ConnectionLost
    result <- runRIO program
    case result of
      Left v ->
        let
          payload =
            Variant.case_
              # Variant.on (Proxy :: Proxy "database") identity
              $ v
        in
          payload `shouldEqual` ConnectionLost
      Right _ -> 1 `shouldEqual` 0

  it "two error types coexist in the same row with distinct tags" do
    let
      program
        :: RIO ()
             ( database :: DatabaseError
             , validation :: ValidationError
             )
             Int
      program = failWith (MissingField "email")
    result <- runRIO program
    case result of
      Left v ->
        let
          payload =
            Variant.case_
              # Variant.on (Proxy :: Proxy "validation") identity
              # Variant.on (Proxy :: Proxy "database")
                  (const (MissingField "wrong-tag"))
              $ v
        in
          payload `shouldEqual` (MissingField "email")
      Right _ -> 1 `shouldEqual` 0

  it "catchTag discharges a FailWith failure by its inferred tag" do
    -- The point of FailWith is that callers don't write a proxy on
    -- the way in, but the row tag still tracks the failure on the
    -- way out, so catchTag finds it.
    let
      handled :: RIO () () String
      handled =
        catchTag (Proxy :: Proxy "database")
          ( case _ of
              ConnectionLost -> pure "recovered from lost connection"
              QueryFailed q -> pure ("retry query: " <> q)
          )
          (failWith ConnectionLost)
    result <- runRIO handled
    result `shouldEqual` (Right "recovered from lost connection" :: Either _ _)
