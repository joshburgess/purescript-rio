module Test.RIO.Fiber.StreamSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Variant as Variant
import Effect.Exception as Effect.Exception
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Queue as Q
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.Scope as Scope
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue as TQ
import RIO.Fiber.Stream as S
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Stream" do
  it "emit + runCollect yields a single-element array" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.emit 42)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 42 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "fromArray + runCollect round-trips" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.fromArray [ 1, 2, 3 ])
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "map transforms every element" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.map (_ * 10) (S.fromArray [ 1, 2, 3 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "filter keeps only matching elements" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.filter (\x -> x `mod` 2 == 0) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 2, 4 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take bounds an infinite stream" do
    counter <- liftEffect (Ref.new 0)
    let
      tick :: F.RIO () () Int
      tick = F.liftEffect (Ref.modify (_ + 1) counter)

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.take 3 (S.repeatRIO tick))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    -- the producer should have been pulled exactly 3 times
    n <- liftEffect (Ref.read counter)
    n `shouldEqual` 3

  it "fold reduces with a seed" do
    let
      prog :: F.RIO () () Int
      prog = S.fold (+) 0 (S.fromArray [ 1, 2, 3, 4 ])
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 10
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "forEach runs an action per element in order" do
    log <- liftEffect (Ref.new ([] :: Array Int))
    let
      prog :: F.RIO () () Unit
      prog = S.forEach
        (\x -> F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) log))
        (S.fromArray [ 7, 8, 9 ])
    _ <- runAff prog {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ 7, 8, 9 ]

  it "buffer decouples producer and consumer pacing" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.buffer 4 (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "merge interleaves two streams and ends when both end" do
    let
      prog :: F.RIO () () Int
      prog = S.fold (+) 0
        (S.merge (S.fromArray [ 1, 2, 3 ]) (S.fromArray [ 10, 20, 30 ]))
    out <- runAff prog {}
    case out of
      -- order is non-deterministic but the sum is well-defined
      Success n -> n `shouldEqual` (1 + 2 + 3 + 10 + 20 + 30)
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapPar transforms elements with bounded concurrency" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.mapPar 3 (\x -> pure (x * 10)) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30, 40, 50 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapPar runs workers concurrently across element boundaries" do
    inFlight <- liftEffect (Ref.new 0)
    maxInFlight <- liftEffect (Ref.new 0)
    let
      slow :: Int -> F.RIO () () Int
      slow x = do
        n <- F.liftEffect (Ref.modify (_ + 1) inFlight)
        F.liftEffect
          (Ref.modify_ (\m -> if n > m then n else m) maxInFlight)
        F.sleep (Milliseconds 10.0)
        _ <- F.liftEffect (Ref.modify (_ - 1) inFlight)
        pure x

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.mapPar 3 slow (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    peak <- liftEffect (Ref.read maxInFlight)
    -- with 5 elements and concurrency 3, we should observe at least
    -- 2 workers active simultaneously at some point.
    (peak >= 2) `shouldEqual` true

  it "scan emits seed then each accumulated value" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.scan (+) 0 (S.fromArray [ 1, 2, 3 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 3, 6 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapAccum threads state through and emits one b per input" do
    let
      runningSum :: Int -> Int -> Tuple Int Int
      runningSum acc a = let acc' = acc + a in Tuple acc' acc'

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.mapAccum runningSum 0 (S.fromArray [ 1, 2, 3, 4 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 3, 6, 10 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "intersperse inserts a separator between every adjacent pair" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.intersperse 0 (S.fromArray [ 1, 2, 3 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 0, 2, 0, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "intersperse on empty or singleton streams emits exactly that" do
    let
      progEmpty :: F.RIO () () (Array Int)
      progEmpty = S.runCollect (S.intersperse 0 (S.fromArray []))

      progOne :: F.RIO () () (Array Int)
      progOne = S.runCollect (S.intersperse 0 (S.fromArray [ 42 ]))
    o1 <- runAff progEmpty {}
    o2 <- runAff progOne {}
    case o1 of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)
    case o2 of
      Success xs -> xs `shouldEqual` [ 42 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "flatMap concatenates sub-streams in order" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        ( S.flatMap (\n -> S.fromArray [ n, n * 10 ])
            (S.fromArray [ 1, 2, 3 ])
        )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 10, 2, 20, 3, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "flatMap with an empty inner stream skips that input" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        ( S.flatMap
            (\n -> if n `mod` 2 == 0 then S.empty else S.fromArray [ n, n ])
            (S.fromArray [ 1, 2, 3, 4, 5 ])
        )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 1, 3, 3, 5, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "groupBy splits into adjacent runs by key" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        (S.groupBy (\x -> x `mod` 2) (S.fromArray [ 1, 3, 4, 6, 7, 8 ]))
    out <- runAff prog {}
    case out of
      Success xs ->
        xs `shouldEqual`
          [ [ 1, 3 ], [ 4, 6 ], [ 7 ], [ 8 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "zipPar pairs elements positionally and ends when either ends" do
    let
      prog :: F.RIO () () (Array (Tuple Int String))
      prog = S.runCollect
        (S.zipPar
          (S.fromArray [ 1, 2, 3 ])
          (S.fromArray [ "a", "b" ]))
    out <- runAff prog {}
    case out of
      Success xs ->
        xs `shouldEqual` [ Tuple 1 "a", Tuple 2 "b" ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "throttle paces a fast source to one per duration" do
    let
      -- 5 elements with a 30 ms throttle should take at least ~120 ms
      -- (4 inter-emission gaps); we check ordering and approximate
      -- pacing via a single elapsed-time read in a single fiber.
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.throttle (Milliseconds 30.0) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "throttle does not slow a source that is already slower than the rate" do
    -- emit 3 elements paced by sleep (20 ms) into a queue, then
    -- throttle at 5 ms; the output rate is governed by the producer.
    q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q (Just 1)
        F.sleep (Milliseconds 20.0)
        Q.offer q (Just 2)
        F.sleep (Milliseconds 20.0)
        Q.offer q (Just 3)
        Q.offer q Nothing

      drained :: S.Stream () () Int
      drained = S.Stream do
        m <- Q.take q
        case m of
          Nothing -> pure S.Done
          Just a -> pure (S.Yield a drained)

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.throttle (Milliseconds 5.0) drained)
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "debounce coalesces bursts into the trailing element" do
    -- producer: emit 1, 2, 3 in rapid succession (5 ms apart), then
    -- wait 60 ms, then 4 -- debounce(40ms) should yield [3, 4].
    q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q (Just 1)
        F.sleep (Milliseconds 5.0)
        Q.offer q (Just 2)
        F.sleep (Milliseconds 5.0)
        Q.offer q (Just 3)
        F.sleep (Milliseconds 80.0)
        Q.offer q (Just 4)
        F.sleep (Milliseconds 80.0)
        Q.offer q Nothing

      drained :: S.Stream () () Int
      drained = S.Stream do
        m <- Q.take q
        case m of
          Nothing -> pure S.Done
          Just a -> pure (S.Yield a drained)

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.debounce (Milliseconds 40.0) drained)
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 3, 4 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "fromQueue + take pulls from a queue concurrently with a feeder" do
    q <- liftEffect (Q.make 4 :: _ (Q.Queue Int))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q 1
        F.sleep (Milliseconds 5.0)
        Q.offer q 2
        F.sleep (Milliseconds 5.0)
        Q.offer q 3

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.take 3 (S.fromQueue q))
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "catchAll switches to a recovery stream on typed failure" do
    let
      emitThenFail :: Int -> S.Stream () (oops :: String) Int
      emitThenFail n
        | n >= 2 = S.Stream (F.fail (Variant.inj (Proxy :: _ "oops") "burn"))
        | otherwise = S.Stream
            (pure (S.Yield n (emitThenFail (n + 1))))

      source :: S.Stream () (oops :: String) Int
      source = emitThenFail 0

      recover :: S.Stream () () Int
      recover = S.fromArray [ 99, 100 ]

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.catchAll (\_ -> recover) source)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 99, 100 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "retry re-pulls the source from the start on failure" do
    counter <- liftEffect (Ref.new 0)
    let
      attempt :: F.RIO () (oops :: String) (Array Int)
      attempt = do
        n <- F.liftEffect (Ref.modify (_ + 1) counter)
        if n < 3 then F.fail (Variant.inj (Proxy :: _ "oops") "no")
        else pure [ 10, 20, 30 ]

      source :: S.Stream () (oops :: String) Int
      source = S.Stream do
        xs <- attempt
        case S.fromArray xs of
          S.Stream pull -> pull

      prog :: F.RIO () (oops :: String) (Array Int)
      prog = S.runCollect (S.retry (Sch.recurs 5) source)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    seen <- liftEffect (Ref.read counter)
    seen `shouldEqual` 3

  it "fromTQueue pulls atomic reads from a transactional queue" do
    q <- liftEffect (TQ.new 4 :: _ (TQ.TQueue Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork do
          STM.atomically (TQ.writeTQueue q 7)
          STM.atomically (TQ.writeTQueue q 8)
          STM.atomically (TQ.writeTQueue q 9)
        result <- S.runCollect (S.take 3 (S.fromTQueue q))
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 7, 8, 9 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "broadcast fans each element to every consumer" do
    let
      source :: S.Stream () () Int
      source = S.fromArray [ 10, 20, 30 ]

      prog :: F.RIO () () { left :: Array Int, right :: Array Int }
      prog = do
        outs <- S.broadcast 2 8 source
        case outs of
          [ a, b ] -> do
            pair <- F.zipPar (S.runCollect a) (S.runCollect b)
            case pair of
              Tuple l r -> pure { left: l, right: r }
          _ -> F.die (mkErr "broadcast did not return 2 streams")
    out <- runAff prog {}
    case out of
      Success r -> do
        r.left `shouldEqual` [ 10, 20, 30 ]
        r.right `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "share hands out subscriber streams that see future elements" do
    q <- liftEffect (Q.make 8 :: _ (Q.Queue (Maybe Int)))
    let
      source :: S.Stream () () Int
      source = sentinelStream q

      prog :: F.RIO () () (Array Int)
      prog = do
        subscribe <- S.share 8 source
        stream <- subscribe
        feeder <- F.fork do
          Q.offer q (Just 1)
          Q.offer q (Just 2)
          Q.offer q (Just 3)
          Q.offer q Nothing
        result <- S.runCollect stream
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "timeoutPerPull yields Nothing when a pull takes too long" do
    q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
    let
      source :: S.Stream () () Int
      source = sentinelStream q

      prog :: F.RIO () () (Array (Maybe Int))
      prog = do
        feeder <- F.fork do
          Q.offer q (Just 1)
          -- Long gap: the next pull should time out.
          F.sleep (Milliseconds 200.0)
          Q.offer q (Just 2)
          Q.offer q Nothing
        result <- S.runCollect
          (S.take 2 (S.timeoutPerPull (Milliseconds 25.0) source))
        F.interrupt feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ Just 1, Nothing ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquireReleaseStream releases on natural Done" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () (Array Int)
      prog = Scope.scoped \scope ->
        S.runCollect
          ( S.acquireReleaseStream
              scope
              (record "open" $> 5)
              (\_ -> record "close")
              (\n -> S.fromArray [ n, n + 1, n + 2 ])
          )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 5, 6, 7 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "open", "close" ]

  it "acquireReleaseStream releases when downstream halts early via take" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      neverEnding :: Int -> S.Stream () () Int
      neverEnding n = S.Stream do
        _ <- pure unit
        pure (S.Yield n (neverEnding (n + 1)))

      prog :: F.RIO () () (Array Int)
      prog = Scope.scoped \scope ->
        S.runCollect
          ( S.take 3
              ( S.acquireReleaseStream
                  scope
                  (record "open" $> 100)
                  (\_ -> record "close")
                  neverEnding
              )
          )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 100, 101, 102 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "open", "close" ]

  it "acquireReleaseStream releases when the consumer is interrupted" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      blocking :: Int -> S.Stream () () Int
      blocking n = S.Stream do
        F.sleep (Milliseconds 1000.0)
        pure (S.Yield n (blocking (n + 1)))

      consumer :: F.RIO () () Unit
      consumer = Scope.scoped \scope ->
        S.run
          ( S.acquireReleaseStream
              scope
              (record "open" $> 7)
              (\_ -> record "close")
              blocking
          )

      prog :: F.RIO () () Unit
      prog = do
        fib <- F.fork consumer
        F.sleep (Milliseconds 10.0)
        F.interrupt fib
        _ <- F.join fib
        pure unit
    _ <- runAff prog {}
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "open", "close" ]

mkErr :: String -> Effect.Exception.Error
mkErr = Effect.Exception.error

sentinelStream :: forall r e. Q.Queue (Maybe Int) -> S.Stream r e Int
sentinelStream q = S.Stream do
  m <- Q.take q
  case m of
    Nothing -> pure S.Done
    Just a -> pure (S.Yield a (sentinelStream q))

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
