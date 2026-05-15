module Test.RIO.Error.TapSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, runRIO, runRIO', sandbox)
import RIO.Error (tapBoth, tapDefect)

spec :: Spec Unit
spec = describe "RIO.Error (tapBoth / tapDefect)" do

  describe "tapBoth" do
    it "fires the onOk arm on success and passes the value through" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () (boom :: Int) Int
        program = tapBoth
          (\_ -> liftEffect (Ref.write "err" probe))
          (\a -> liftEffect (Ref.write ("ok " <> show a) probe))
          (pure 7)
      result <- runRIO program
      result `shouldEqual` (Right 7 :: Either _ _)
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "ok 7"

    it "fires the onErr arm on typed failure and re-raises it" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () (boom :: Int) Int
        program = tapBoth
          ( \v ->
              let
                payload =
                  Variant.case_ # Variant.on (Proxy :: Proxy "boom") identity $ v
              in
                liftEffect (Ref.write ("err " <> show payload) probe)
          )
          (\_ -> liftEffect (Ref.write "ok" probe))
          (fail (Proxy :: Proxy "boom") 99)
      result <- runRIO program
      case result of
        Left v -> do
          let payload = Variant.case_ # Variant.on (Proxy :: Proxy "boom") identity $ v
          payload `shouldEqual` 99
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "err 99"

    it "lets a handler failure replace the original outcome" do
      let
        program :: RIO () (boom :: Int, replaced :: String) Int
        program = tapBoth
          (\_ -> pure unit)
          (\_ -> fail (Proxy :: Proxy "replaced") "from-handler")
          (pure 1 :: RIO () (boom :: Int, replaced :: String) Int)
      result <- runRIO program
      case result of
        Left v ->
          let
            payload =
              Variant.case_
                # Variant.on (Proxy :: Proxy "replaced") identity
                # Variant.on (Proxy :: Proxy "boom") show
                $ v
          in
            payload `shouldEqual` "from-handler"
        Right _ -> 1 `shouldEqual` 0

  describe "tapDefect" do
    it "passes a success through without invoking the handler" do
      probe <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Int
        program = tapDefect
          (\_ -> liftEffect (Ref.modify_ (_ + 1) probe))
          (pure 5)
      result <- runRIO' program
      result `shouldEqual` 5
      fired <- liftEffect (Ref.read probe)
      fired `shouldEqual` 0

    it "passes a typed failure through without invoking the handler" do
      probe <- liftEffect (Ref.new 0)
      let
        program :: RIO () (boom :: Int) Int
        program = tapDefect
          (\_ -> liftEffect (Ref.modify_ (_ + 1) probe))
          (fail (Proxy :: Proxy "boom") 99)
      result <- runRIO program
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
      fired <- liftEffect (Ref.read probe)
      fired `shouldEqual` 0

    it "fires the handler on a defect and re-raises it (observable via sandbox)" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () () (Either _ Int)
        program = sandbox
          ( tapDefect
              ( \err ->
                  liftEffect (Ref.write ("saw " <> Exception.message err) probe)
              )
              (die (Exception.error "kapow"))
          )
      result <- runRIO' program
      case result of
        Left err -> Exception.message err `shouldEqual` "kapow"
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "saw kapow"
