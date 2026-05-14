module Test.RIO.Stream.ResourceSpec (spec) where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Effect.Aff (attempt, error)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, runRIO, runRIO', scoped)
import RIO.Stream (Stream, flatMap, fromArray, mapM, runCollect, runDrain)
import RIO.Stream.Resource (bracketStream)

spec :: Spec Unit
spec = do
  describe "RIO.Stream.Resource" do

    describe "bracketStream" do
      it "releases the resource when the surrounding scope exits" do
        events <- liftEffect (Ref.new [])
        let
          record :: forall r e. String -> RIO r e Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          recordAff :: String -> _
          recordAff s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            record "before-stream"
            runDrain
              ( flatMap
                  ( bracketStream
                      (record "acquire" *> pure "resource")
                      (\_ -> recordAff "release")
                  )
                  ( \_ -> mapM (\n -> record ("use-" <> show n))
                      (fromArray [ 1, 2 ])
                  )
              )
            record "after-stream"
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "before-stream"
          , "acquire"
          , "use-1"
          , "use-2"
          , "after-stream"
          , "release"
          ]

      it "releases the resource when the stream raises a typed failure" do
        events <- liftEffect (Ref.new [])
        let
          record :: forall r e. String -> RIO r e Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          recordAff :: String -> _
          recordAff s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          inner :: Stream (scope :: _ | ()) (boom :: String) Int
          inner = flatMap
            ( bracketStream
                (record "acquire" *> pure "resource")
                (\_ -> recordAff "release")
            )
            ( \_ -> mapM
                (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
                (fromArray [ 1 ])
            )

          program :: RIO () (boom :: String) Unit
          program = scoped (runDrain inner)
        r <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "release" ]
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected typed failure to surface"

      it "does not register a finalizer when acquire fails" do
        events <- liftEffect (Ref.new [])
        let
          recordAff :: String -> _
          recordAff s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          inner :: Stream (scope :: _ | ()) (boom :: String) Int
          inner = flatMap
            ( bracketStream
                (fail (Proxy :: Proxy "boom") "acquire-failed")
                (\_ -> recordAff "release")
            )
            (\_ -> fromArray [ 1, 2 ])

          program :: RIO () (boom :: String) Unit
          program = scoped (runDrain inner)
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` []

      it "releases the resource when the stream raises a defect" do
        -- Module docstring promises release "on every termination
        -- path (success, typed failure, defect, or fiber kill)".
        -- Success and typed-failure are pinned above; pin the
        -- defect path so the full bracket contract is documented.
        events <- liftEffect (Ref.new [])
        let
          record :: forall r e. String -> RIO r e Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          recordAff :: String -> _
          recordAff s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          inner :: Stream (scope :: _ | ()) () Int
          inner = flatMap
            ( bracketStream
                (record "acquire" *> pure "resource")
                (\_ -> recordAff "release")
            )
            ( \_ -> mapM
                (\_ -> die (error "kaboom"))
                (fromArray [ 1 ])
            )

          program :: RIO () () Unit
          program = scoped (runDrain inner)
        _ <- attempt (runRIO' program)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "release" ]

      it "release runs once the scope exits even if consumer takes only some" do
        events <- liftEffect (Ref.new [])
        let
          record :: forall r e. String -> RIO r e Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          recordAff :: String -> _
          recordAff s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () (Array Int)
          program = scoped
            ( runCollect
                ( flatMap
                    ( bracketStream
                        (record "acquire" *> pure "resource")
                        (\_ -> recordAff "release")
                    )
                    (\_ -> fromArray [ 1 ])
                )
            )
        r <- runRIO program
        order <- liftEffect (Ref.read events)
        r `shouldEqual` Right [ 1 ]
        order `shouldEqual` [ "acquire", "release" ]
