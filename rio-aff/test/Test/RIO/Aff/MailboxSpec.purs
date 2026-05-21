module Test.RIO.Aff.MailboxSpec (spec) where

import Prelude hiding (join)

import Data.Array (length, replicate, sort, (..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import RIO.Aff.Concurrency (forkScoped, join)
import RIO.Aff.Core (RIO, ask, runRIO)
import RIO.Aff.Mailbox as Mailbox
import RIO.Aff.Resource (scoped)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "RIO.Aff.Mailbox" do
  it "delivers a value offered by the single producer" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      program :: RIO () () (Maybe Int)
      program = do
        Mailbox.offer mb 7
        Mailbox.take mb
    result <- runRIO program
    result `shouldEqual` (Right (Just 7) :: Either _ _)

  it "yields Nothing after the sole producer signals done" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      program :: RIO () () (Maybe Int)
      program = do
        Mailbox.done mb
        Mailbox.take mb
    result <- runRIO program
    result `shouldEqual` (Right Nothing :: Either _ _)

  it "stays closed: every take after Nothing is Nothing" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      program :: RIO () () (Array (Maybe Int))
      program = do
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) [ unit, unit, unit ]
    result <- runRIO program
    result `shouldEqual` (Right [ Nothing, Nothing, Nothing ] :: Either _ _)

  it "drains buffered values before reporting closed" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      program :: RIO () () (Array (Maybe Int))
      program = do
        traverse_ (Mailbox.offer mb) [ 1, 2, 3 ]
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) [ unit, unit, unit, unit ]
    result <- runRIO program
    result `shouldEqual`
      (Right [ Just 1, Just 2, Just 3, Nothing ] :: Either _ _)

  it "closes only after every declared producer has signalled done" do
    mb <- liftEffect (Mailbox.make 4 3 :: _ (Mailbox.Mailbox Int))
    log <- liftEffect (ERef.new Nothing)
    let
      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        Mailbox.done mb
        Mailbox.done mb
        consumer <- forkScoped scope do
          m <- Mailbox.take mb
          liftEffect (ERef.write (Just m) log)
        -- third done unblocks the consumer
        Mailbox.done mb
        join consumer
    result <- runRIO program
    case result of
      Right _ -> do
        got <- liftEffect (ERef.read log)
        got `shouldEqual` Just Nothing
      Left _ -> 1 `shouldEqual` 0

  it "merges multiple concurrent producers into one consumer" do
    mb <- liftEffect (Mailbox.make 16 3 :: _ (Mailbox.Mailbox Int))
    let
      n = 10

      producer :: forall r. Int -> RIO r () Unit
      producer base = do
        traverse_ (\i -> Mailbox.offer mb (base + i)) (1 .. n)
        Mailbox.done mb

      drain :: forall r. Array Int -> RIO r () (Array Int)
      drain acc = do
        m <- Mailbox.take mb
        case m of
          Nothing -> pure acc
          Just x -> drain (acc <> [ x ])

      program :: RIO () () (Array Int)
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        _ <- forkScoped scope (producer 0)
        _ <- forkScoped scope (producer 100)
        _ <- forkScoped scope (producer 200)
        drain []
    result <- runRIO program
    case result of
      Right xs -> do
        length xs `shouldEqual` (3 * n)
        sort xs `shouldEqual`
          sort
            ( (1 .. n)
                <> map (_ + 100) (1 .. n)
                <> map (_ + 200) (1 .. n)
            )
      Left _ -> 1 `shouldEqual` 0

  it "unbounded never suspends the producer" do
    mb <- liftEffect (Mailbox.unbounded 1 :: _ (Mailbox.Mailbox Int))
    let
      m = 50
      program :: RIO () () (Array (Maybe Int))
      program = do
        traverse_ (Mailbox.offer mb) (1 .. m)
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) (replicate (m + 1) unit)
    result <- runRIO program
    case result of
      Right ms -> do
        length ms `shouldEqual` (m + 1)
        ms `shouldEqual` (map Just (1 .. m) <> [ Nothing ])
      Left _ -> 1 `shouldEqual` 0
