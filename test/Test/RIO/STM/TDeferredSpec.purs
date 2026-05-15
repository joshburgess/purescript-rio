module Test.RIO.STM.TDeferredSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fork, runRIO)
import RIO.STM (atomically)
import RIO.STM.TDeferred
  ( awaitTDeferred
  , failTDeferred
  , makeTDeferred
  , pollTDeferred
  , succeedTDeferred
  , tryAwaitTDeferred
  )

spec :: Spec Unit
spec = describe "RIO.STM.TDeferred" do
  it "succeedTDeferred fills an empty cell and returns True" do
    let
      program :: RIO () () Boolean
      program = atomically do
        d <- makeTDeferred :: _ (_ () Int)
        succeedTDeferred d 7
    result <- runRIO program
    result `shouldEqual` (Right true :: Either _ Boolean)

  it "succeedTDeferred returns False on a second fill (no overwrite)" do
    let
      program
        :: RIO () ()
             { firstOk :: Boolean, secondOk :: Boolean, value :: Int }
      program = atomically do
        d <- makeTDeferred :: _ (_ () Int)
        firstOk <- succeedTDeferred d 1
        secondOk <- succeedTDeferred d 999
        value <- awaitTDeferred d
        pure { firstOk, secondOk, value }
    result <- runRIO program
    result `shouldEqual`
      ( Right { firstOk: true, secondOk: false, value: 1 }
          :: Either _ _
      )

  it "awaitTDeferred returns a stored success value" do
    let
      program :: RIO () () Int
      program = atomically do
        d <- makeTDeferred :: _ (_ () Int)
        _ <- succeedTDeferred d 42
        awaitTDeferred d
    result <- runRIO program
    result `shouldEqual` (Right 42 :: Either _ Int)

  it "awaitTDeferred raises a stored typed failure on the row" do
    -- A failed TDeferred surfaces its stored Variant on the
    -- awaiter's row via `throwSTM`; the unhandled `boom` tag stays
    -- on the row.
    let
      program :: RIO () (boom :: String) Int
      program = atomically do
        d :: _ (boom :: String) Int <- makeTDeferred
        _ <- failTDeferred d
          (Variant.inj (Proxy :: Proxy "boom") "exploded")
        awaitTDeferred d
    result <- runRIO program
    case result of
      Left v ->
        let
          msg =
            Variant.case_
              # Variant.on (Proxy :: Proxy "boom") identity
              $ v
        in
          msg `shouldEqual` "exploded"
      Right _ -> 1 `shouldEqual` 0

  it "pollTDeferred returns Nothing while empty, Just after fill" do
    let
      program
        :: RIO () ()
             { before :: Maybe (Either (Variant ()) Int)
             , after :: Maybe (Either (Variant ()) Int)
             }
      program = atomically do
        d <- makeTDeferred :: _ (_ () Int)
        before <- pollTDeferred d
        _ <- succeedTDeferred d 99
        after <- pollTDeferred d
        pure { before, after }
    result <- runRIO program
    result `shouldEqual`
      ( Right { before: Nothing, after: Just (Right 99) }
          :: Either _ _
      )

  it "tryAwaitTDeferred returns Nothing on empty (no retry)" do
    let
      program :: RIO () () (Maybe Int)
      program = atomically do
        d <- makeTDeferred :: _ (_ () Int)
        tryAwaitTDeferred d
    result <- runRIO program
    result `shouldEqual` (Right Nothing :: Either _ (Maybe Int))

  it "awaitTDeferred retries until a fork fills the cell" do
    -- A fork delays then commits a fill; the awaiting transaction
    -- blocks on retry until that commit wakes the read set.
    let
      program :: RIO () () Int
      program = do
        d <- atomically (makeTDeferred :: _ (_ () Int))
        _ <- fork do
          liftAff (delay (Milliseconds 5.0))
          _ <- atomically (succeedTDeferred d 123)
          pure unit
        atomically (awaitTDeferred d)
    result <- runRIO program
    result `shouldEqual` (Right 123 :: Either _ Int)

  it "fill composes inside one atomically block (atomic commit)" do
    -- The headline use case: an awaiter wakes when a fork commits
    -- the fill alongside any other transactional state changes;
    -- here we just confirm the fill is observable post-commit.
    let
      program :: RIO () () Int
      program = do
        d <- atomically (makeTDeferred :: _ (_ () Int))
        _ <- atomically do
          _ <- succeedTDeferred d 10
          pure unit
        atomically (awaitTDeferred d)
    result <- runRIO program
    result `shouldEqual` (Right 10 :: Either _ Int)
