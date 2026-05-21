module Test.RIO.Fiber.MailboxSpec (spec) where

import Prelude

import Data.Array (length, replicate, sort, (..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Mailbox as Mailbox
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Mailbox" do
  it "delivers a value offered by the single producer" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = do
        Mailbox.offer mb 7
        Mailbox.take mb
    out <- runAff prog {}
    case out of
      Success m -> m `shouldEqual` Just 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "yields Nothing after the sole producer signals done" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = do
        Mailbox.done mb
        Mailbox.take mb
    out <- runAff prog {}
    case out of
      Success m -> m `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "stays closed: every take after Nothing is Nothing" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      prog :: F.RIO () () (Array (Maybe Int))
      prog = do
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) [ unit, unit, unit ]
    out <- runAff prog {}
    case out of
      Success ms -> ms `shouldEqual` [ Nothing, Nothing, Nothing ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "drains buffered values before reporting closed" do
    mb <- liftEffect (Mailbox.make 4 1 :: _ (Mailbox.Mailbox Int))
    let
      prog :: F.RIO () () (Array (Maybe Int))
      prog = do
        traverse_ (Mailbox.offer mb) [ 1, 2, 3 ]
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) [ unit, unit, unit, unit ]
    out <- runAff prog {}
    case out of
      Success ms ->
        ms `shouldEqual` [ Just 1, Just 2, Just 3, Nothing ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "closes only after every declared producer has signalled done" do
    mb <- liftEffect (Mailbox.make 4 3 :: _ (Mailbox.Mailbox Int))
    log <- liftEffect (Ref.new Nothing)
    let
      prog :: F.RIO () () Unit
      prog = do
        Mailbox.done mb
        Mailbox.done mb
        consumer <- F.fork do
          m <- Mailbox.take mb
          F.liftEffect (Ref.write (Just m) log)
        -- third done unblocks the consumer
        Mailbox.done mb
        F.join consumer
    out <- runAff prog {}
    case out of
      Success _ -> do
        got <- liftEffect (Ref.read log)
        got `shouldEqual` Just Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "merges multiple concurrent producers into one consumer" do
    mb <- liftEffect (Mailbox.make 16 3 :: _ (Mailbox.Mailbox Int))
    let
      n = 10
      producer base = do
        traverse_ (\i -> Mailbox.offer mb (base + i)) (1 .. n)
        Mailbox.done mb

      drain acc = do
        m <- Mailbox.take mb
        case m of
          Nothing -> pure acc
          Just x -> drain (acc <> [ x ])

      prog :: F.RIO () () (Array Int)
      prog = do
        _ <- F.fork (producer 0)
        _ <- F.fork (producer 100)
        _ <- F.fork (producer 200)
        drain []
    out <- runAff prog {}
    case out of
      Success xs -> do
        length xs `shouldEqual` (3 * n)
        sort xs `shouldEqual`
          sort
            ( (1 .. n)
                <> map (_ + 100) (1 .. n)
                <> map (_ + 200) (1 .. n)
            )
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "unbounded never suspends the producer" do
    mb <- liftEffect (Mailbox.unbounded 1 :: _ (Mailbox.Mailbox Int))
    let
      m = 50
      prog :: F.RIO () () (Array (Maybe Int))
      prog = do
        traverse_ (Mailbox.offer mb) (1 .. m)
        Mailbox.done mb
        traverse (\_ -> Mailbox.take mb) (replicate (m + 1) unit)
    out <- runAff prog {}
    case out of
      Success ms -> do
        length ms `shouldEqual` (m + 1)
        ms `shouldEqual`
          (map Just (1 .. m) <> [ Nothing ])
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
