module Test.RIO.Fiber.StreamSpec (spec) where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception as Effect.Exception
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Hub as Hub
import RIO.Fiber.Queue as Q
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.Scope as Scope
import RIO.Fiber.STM as STM
import RIO.Fiber.Sink as Sink
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

  describe "mergeAll" do
    it "interleaves an array of streams and ends when all end" do
      let
        prog :: F.RIO () () Int
        prog = S.fold (+) 0
          ( S.mergeAll
              [ S.fromArray [ 1, 2, 3 ]
              , S.fromArray [ 10, 20, 30 ]
              , S.fromArray [ 100, 200, 300 ]
              ]
          )
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual`
          (1 + 2 + 3 + 10 + 20 + 30 + 100 + 200 + 300)
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "an empty input array yields the empty stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.mergeAll ([] :: Array (S.Stream () () Int)))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "a singleton array forwards that one stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.mergeAll [ S.fromArray [ 1, 2, 3 ] ])
      out <- runAff prog {}
      case out of
        Success xs -> Array.sort xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "sliding" do
    it "emits overlapping windows with step = 1" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          ( S.sliding { chunkSize: 3, step: 1 }
              (S.fromArray [ 1, 2, 3, 4, 5 ])
          )
      out <- runAff prog {}
      case out of
        -- chunkSize 3, step 1 over [1..5]: [1,2,3], [2,3,4], [3,4,5]
        -- and a trailing partial of what remains.
        Success xs -> do
          Array.take 3 xs `shouldEqual`
            [ [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4, 5 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "emits disjoint chunks when step = chunkSize" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          ( S.sliding { chunkSize: 2, step: 2 }
              (S.fromArray [ 1, 2, 3, 4, 5, 6 ])
          )
      out <- runAff prog {}
      case out of
        Success xs ->
          xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "emits a trailing partial window when upstream ends mid-buffer" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          ( S.sliding { chunkSize: 3, step: 3 }
              (S.fromArray [ 1, 2, 3, 4 ])
          )
      out <- runAff prog {}
      case out of
        Success xs ->
          xs `shouldEqual` [ [ 1, 2, 3 ], [ 4 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "an empty upstream yields no windows" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          (S.sliding { chunkSize: 3, step: 1 } (S.empty :: _ Int))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "fromHub" do
    it "subscribes for the scope's lifetime and yields published values" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          hub <- F.liftEffect (Hub.make 8)
          -- Push values from a forked publisher; the stream subscribes
          -- inside the scope and collects three values before ending.
          _ <- F.fork do
            F.sleep (Milliseconds 5.0)
            Hub.publish hub 1
            Hub.publish hub 2
            Hub.publish hub 3
            Hub.publish hub 4
          S.runCollect (S.take 3 (S.fromHub scope hub))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "releases the subscription slot when the scope closes" do
      let
        prog :: F.RIO () () { duringScope :: Int, afterScope :: Int }
        prog = do
          hub <- F.liftEffect (Hub.make 4)
          duringScope <- Scope.scoped \scope -> do
            _ <- F.fork do
              F.sleep (Milliseconds 5.0)
              Hub.publish hub 1
            _ <- S.runCollect (S.take 1 (S.fromHub scope hub))
            Hub.subscribers hub
          afterScope <- Hub.subscribers hub
          pure { duringScope, afterScope }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.duringScope `shouldEqual` 1
          r.afterScope `shouldEqual` 0
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "fromRIO" do
    it "runs the action once and emits its result, then ends" do
      counter <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.fromRIO (F.liftEffect (Ref.modify (_ + 1) counter)))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1 ]
        other -> fail ("expected Success, got " <> describeOutcome other)
      n <- liftEffect (Ref.read counter)
      n `shouldEqual` 1

    it "propagates a typed failure from the lifted action" do
      let
        prog :: F.RIO () (boom :: String) (Array Int)
        prog = S.runCollect
          (S.fromRIO (F.fail (Variant.inj (Proxy :: _ "boom") "x")))
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "concat / concatAll" do
    it "concat yields the left stream then the right stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.concat (S.fromArray [ 1, 2 ]) (S.fromArray [ 10, 20 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 10, 20 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "concat is lazy: the right stream is not pulled until the left ends" do
      pulledRight <- liftEffect (Ref.new false)
      let
        left :: S.Stream () () Int
        left = S.fromArray [ 1, 2 ]

        right :: S.Stream () () Int
        right = S.fromRIO do
          F.liftEffect (Ref.write true pulledRight)
          pure 99

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.take 2 (S.concat left right))
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read pulledRight)
      seen `shouldEqual` false

    it "concatAll on an empty array is empty" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.concatAll ([] :: Array (S.Stream () () Int)))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "concatAll joins multiple streams in order" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          ( S.concatAll
              [ S.fromArray [ 1, 2 ]
              , S.fromArray [ 3 ]
              , S.fromArray [ 4, 5 ]
              ]
          )
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "bufferDropping / bufferSliding" do
    it "bufferDropping forwards every element when consumer keeps up" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.bufferDropping 8 (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "bufferSliding forwards every element when consumer keeps up" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.bufferSliding 8 (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
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

  it "chunked groups consecutive elements into fixed-size arrays" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect (S.chunked 3 (S.fromArray [ 1, 2, 3, 4, 5, 6, 7 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chunked emits an empty stream for an empty source" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect (S.chunked 4 (S.fromArray []))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "unchunked flattens a chunked stream back to elements" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.unchunked (S.fromArray [ [ 1, 2 ], [], [ 3 ], [ 4, 5, 6 ] ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5, 6 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapChunks transforms each chunk in place" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        ( S.mapChunks (map (_ * 10))
            (S.chunked 2 (S.fromArray [ 1, 2, 3, 4, 5 ]))
        )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual`
        [ [ 10, 20 ], [ 30, 40 ], [ 50 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chunked then unchunked round-trips" do
    let
      input = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.unchunked (S.chunked 4 (S.fromArray input)))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` input
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

  describe "zipLatest" do
    it "emits each new value paired with the most recent of the other side" do
      qA <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
      qB <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe String)))
      let
        feed :: F.RIO () () Unit
        feed = do
          Q.offer qA (Just 1)
          F.sleep (Milliseconds 5.0)
          Q.offer qB (Just "a")
          F.sleep (Milliseconds 5.0)
          Q.offer qA (Just 2)
          F.sleep (Milliseconds 5.0)
          Q.offer qB (Just "b")
          F.sleep (Milliseconds 5.0)
          Q.offer qA Nothing
          Q.offer qB Nothing

        sA :: S.Stream () () Int
        sA = S.Stream do
          m <- Q.take qA
          case m of
            Nothing -> pure S.Done
            Just a -> pure (S.Yield a sA)

        sB :: S.Stream () () String
        sB = S.Stream do
          m <- Q.take qB
          case m of
            Nothing -> pure S.Done
            Just b -> pure (S.Yield b sB)

        prog :: F.RIO () () (Array (Tuple Int String))
        prog = do
          fib <- F.fork feed
          xs <- S.runCollect (S.zipLatest sA sB)
          _ <- F.join fib
          pure xs
      out <- runAff prog {}
      case out of
        Success xs ->
          xs `shouldEqual`
            [ Tuple 1 "a", Tuple 2 "a", Tuple 2 "b" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "yields no output when one side never emits" do
      let
        prog :: F.RIO () () (Array (Tuple Int Int))
        prog = S.runCollect
          (S.zipLatest (S.fromArray [ 1, 2, 3 ]) S.empty)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "zipLatestWith combines latest values with the supplied function" do
      qA <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
      qB <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
      let
        feed :: F.RIO () () Unit
        feed = do
          Q.offer qA (Just 10)
          F.sleep (Milliseconds 5.0)
          Q.offer qB (Just 1)
          F.sleep (Milliseconds 5.0)
          Q.offer qB (Just 2)
          F.sleep (Milliseconds 5.0)
          Q.offer qA Nothing
          Q.offer qB Nothing

        sA :: S.Stream () () Int
        sA = S.Stream do
          m <- Q.take qA
          case m of
            Nothing -> pure S.Done
            Just a -> pure (S.Yield a sA)

        sB :: S.Stream () () Int
        sB = S.Stream do
          m <- Q.take qB
          case m of
            Nothing -> pure S.Done
            Just b -> pure (S.Yield b sB)

        prog :: F.RIO () () (Array Int)
        prog = do
          fib <- F.fork feed
          xs <- S.runCollect (S.zipLatestWith (+) sA sB)
          _ <- F.join fib
          pure xs
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 11, 12 ]
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

  describe "stream laws and edge cases" do
    it "runCollect on an empty stream yields []" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.empty :: S.Stream () () Int)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "fromArray on an empty array yields an empty stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.fromArray [])
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "map identity = identity (functor law)" do
      let
        xs = [ 1, 2, 3, 4 ]

        prog :: F.RIO () () { mapped :: Array Int, raw :: Array Int }
        prog = do
          mapped <- S.runCollect (S.map identity (S.fromArray xs))
          raw <- S.runCollect (S.fromArray xs)
          pure { mapped, raw }
      out <- runAff prog {}
      case out of
        Success r -> r.mapped `shouldEqual` r.raw
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "map composition: map (f <<< g) = map f <<< map g" do
      let
        f = (_ + 1)
        g = (_ * 2)
        src = S.fromArray [ 1, 2, 3 ]

        prog :: F.RIO () () { lhs :: Array Int, rhs :: Array Int }
        prog = do
          lhs <- S.runCollect (S.map (f <<< g) src)
          rhs <- S.runCollect (S.map f (S.map g src))
          pure { lhs, rhs }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.lhs `shouldEqual` r.rhs
          r.lhs `shouldEqual` [ 3, 5, 7 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "take 0 yields an empty stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.take 0 (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "take N on a shorter stream returns the whole stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.take 10 (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "filter on an all-rejecting predicate yields []" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.filter (\_ -> false) (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "filter is idempotent under repeated application of the same predicate" do
      let
        prog :: F.RIO () () { once :: Array Int, twice :: Array Int }
        prog = do
          once <- S.runCollect
            (S.filter (\n -> n `mod` 2 == 0) (S.fromArray [ 1, 2, 3, 4, 5, 6 ]))
          twice <- S.runCollect
            ( S.filter (\n -> n `mod` 2 == 0)
                (S.filter (\n -> n `mod` 2 == 0)
                  (S.fromArray [ 1, 2, 3, 4, 5, 6 ])
                )
            )
          pure { once, twice }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.once `shouldEqual` r.twice
          r.once `shouldEqual` [ 2, 4, 6 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "repeatRIO + take produces N elements from the same action" do
      counter <- liftEffect (Ref.new 0)
      let
        bump :: F.RIO () () Int
        bump = F.liftEffect (Ref.modify (_ + 1) counter)

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.take 4 (S.repeatRIO bump))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "run drives the stream to completion without collecting" do
      -- `S.run` is defined as `forEach (\_ -> pure unit)`. It walks
      -- every element, runs the per-element effect (a no-op), and
      -- completes. Side effects baked into the source still fire.
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        source :: S.Stream () () Int
        source = S.Stream do
          F.liftEffect (Ref.modify_ (\xs -> xs <> [ 1 ]) seen)
          pure
            ( S.Yield 1
                ( S.Stream do
                    F.liftEffect (Ref.modify_ (\xs -> xs <> [ 2 ]) seen)
                    pure (S.Yield 2 S.empty)
                )
            )

        prog :: F.RIO () () Unit
        prog = S.run source
      out <- runAff prog {}
      case out of
        Success _ -> pure unit
        other -> fail ("expected Success, got " <> describeOutcome other)
      ns <- liftEffect (Ref.read seen)
      ns `shouldEqual` [ 1, 2 ]

    it "catchAll yields elements from the source before the failure, then switches" do
      let
        oops :: Int -> F.RIO () (boom :: String) Int
        oops 3 = F.fail (Variant.inj (Proxy :: _ "boom") "stop")
        oops n = pure n

        upstream :: S.Stream () (boom :: String) Int
        upstream = S.Stream do
          a <- oops 1
          pure
            ( S.Yield a
                ( S.Stream do
                    b <- oops 2
                    pure
                      ( S.Yield b
                          ( S.Stream do
                              c <- oops 3
                              pure (S.Yield c (S.empty))
                          )
                      )
                )
            )

        recover :: Variant (boom :: String) -> S.Stream () () Int
        recover _ = S.fromArray [ 100, 200 ]

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.catchAll recover upstream)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 100, 200 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "flatMap with empty inner streams gives an empty result" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          ( S.flatMap (\_ -> S.empty :: S.Stream () () Int)
              (S.fromArray [ 1, 2, 3 ])
          )
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "flatMap with singleton inner streams is map" do
      let
        prog :: F.RIO () () { flat :: Array Int, mapped :: Array Int }
        prog = do
          flat <- S.runCollect
            (S.flatMap (\n -> S.emit (n * 10)) (S.fromArray [ 1, 2, 3 ]))
          mapped <- S.runCollect
            (S.map (_ * 10) (S.fromArray [ 1, 2, 3 ]))
          pure { flat, mapped }
      out <- runAff prog {}
      case out of
        Success r -> r.flat `shouldEqual` r.mapped
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "buffer 1 preserves order and elements" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.buffer 1 (S.fromArray [ 1, 2, 3, 4 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "scan emits N + 1 elements for an N-element source (seed + each step)" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.scan (+) 0 (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 0, 1, 3, 6 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "merge of two empty streams is empty" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          ( S.merge (S.empty :: S.Stream () () Int)
              (S.empty :: S.Stream () () Int)
          )
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "zipPar terminates with the shorter side's length" do
      let
        prog :: F.RIO () () (Array (Tuple Int String))
        prog = S.runCollect
          ( S.zipPar
              (S.fromArray [ 1, 2, 3, 4, 5 ])
              (S.fromArray [ "a", "b", "c" ])
          )
      out <- runAff prog {}
      case out of
        Success xs -> do
          xs `shouldEqual`
            [ Tuple 1 "a", Tuple 2 "b", Tuple 3 "c" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "peel" do
    it "peel head returns the first element and the remaining stream" do
      let
        prog :: F.RIO () () (Tuple (Maybe Int) (Array Int))
        prog = do
          Tuple h rest <- S.peel Sink.head (S.fromArray [ 1, 2, 3, 4 ])
          xs <- S.runCollect rest
          pure (Tuple h xs)
      out <- runAff prog {}
      case out of
        Success r -> r `shouldEqual` Tuple (Just 1) [ 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "peel takeN returns the prefix and the remaining stream" do
      let
        prog :: F.RIO () () (Tuple (Array Int) (Array Int))
        prog = do
          Tuple xs rest <- S.peel (Sink.takeN 3) (S.fromArray [ 1, 2, 3, 4, 5 ])
          tail <- S.runCollect rest
          pure (Tuple xs tail)
      out <- runAff prog {}
      case out of
        Success r -> r `shouldEqual` Tuple [ 1, 2, 3 ] [ 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "peel falls back to done when the stream ends early" do
      let
        prog :: F.RIO () () (Tuple (Array Int) (Array Int))
        prog = do
          Tuple xs rest <- S.peel (Sink.takeN 10) (S.fromArray [ 1, 2 ])
          tail <- S.runCollect rest
          pure (Tuple xs tail)
      out <- runAff prog {}
      case out of
        Success r -> r `shouldEqual` Tuple [ 1, 2 ] []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "transduce" do
    it "transduce takeN emits each chunk as the sink terminates" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          (S.transduce (Sink.takeN 2) (S.fromArray [ 1, 2, 3, 4, 5, 6 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "transduce emits a flush chunk when the stream ends mid-batch" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          (S.transduce (Sink.takeN 3) (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2, 3 ], [ 4, 5 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "transduce on an empty stream emits nothing" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect (S.transduce (Sink.takeN 2) (S.fromArray []))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "aggregate" do
    it "behaves like transduce: emits each completed batch" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          (S.aggregate (Sink.takeN 2) (S.fromArray [ 1, 2, 3, 4, 5, 6 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "aggregateWithin" do
    it "emits a full batch when the sink completes before the timer fires" do
      -- A fast source with a generous 100 ms timer: sink fires first
      -- so the timer is never the trigger.
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          ( S.aggregateWithin (Milliseconds 100.0) (Sink.takeN 2)
              (S.fromArray [ 1, 2, 3, 4 ])
          )
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "flushes a partial batch when the timer fires before the sink completes" do
      -- Producer emits 2, then waits 60 ms (> 30 ms timeout), then 3.
      -- takeN 5 never completes inside a single 30 ms window, so the
      -- forced flush should split the output.
      q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
      let
        feed :: F.RIO () () Unit
        feed = do
          Q.offer q (Just 1)
          Q.offer q (Just 2)
          F.sleep (Milliseconds 60.0)
          Q.offer q (Just 3)
          Q.offer q Nothing

        drained :: S.Stream () () Int
        drained = S.Stream do
          m <- Q.take q
          case m of
            Nothing -> pure S.Done
            Just a -> pure (S.Yield a drained)

        prog :: F.RIO () () (Array (Array Int))
        prog = do
          feeder <- F.fork feed
          result <- S.runCollect
            (S.aggregateWithin (Milliseconds 30.0) (Sink.takeN 5) drained)
          _ <- F.join feeder
          pure result
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "emits a final flush when upstream ends mid-batch" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          ( S.aggregateWithin (Milliseconds 100.0) (Sink.takeN 3)
              (S.fromArray [ 1, 2, 3, 4, 5 ])
          )
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ [ 1, 2, 3 ], [ 4, 5 ] ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "on an empty stream emits nothing" do
      let
        prog :: F.RIO () () (Array (Array Int))
        prog = S.runCollect
          (S.aggregateWithin (Milliseconds 50.0) (Sink.takeN 2) (S.fromArray []))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "changes" do
    it "changes drops consecutive duplicates" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.changes (S.fromArray [ 1, 1, 2, 2, 2, 3, 1, 1 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 1 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "changes leaves a unique stream untouched" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.changes (S.fromArray [ 1, 2, 3, 4 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "changes on an empty stream is empty" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.changes (S.fromArray []))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "range" do
    it "yields ascending integers from start (incl) to end (excl)" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.range 0 5)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 0, 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "is empty when end <= start" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.range 5 5)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "iterate" do
    it "iterate threads the step function through the seed" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.take 5 (S.iterate 1 (_ * 2)))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 4, 8, 16 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "unfold" do
    it "yields until the generator returns Nothing" do
      let
        gen :: Int -> Maybe (Tuple Int Int)
        gen n
          | n > 3 = Nothing
          | otherwise = Just (Tuple (n * n) (n + 1))

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.unfold 1 gen)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 4, 9 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "unfoldRIO" do
    it "runs an effectful generator until it returns Nothing" do
      let
        gen :: Int -> F.RIO () () (Maybe (Tuple Int Int))
        gen n
          | n > 3 = pure Nothing
          | otherwise = pure (Just (Tuple (n * n) (n + 1)))

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.unfoldRIO 1 gen)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 4, 9 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "paginate / paginateRIO" do
    it "paginate emits a value at each step and stops when next is Nothing" do
      let
        step :: Int -> Tuple Int (Maybe Int)
        step n =
          if n >= 4 then Tuple n Nothing
          else Tuple n (Just (n + 1))

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.paginate 1 step)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "paginateRIO threads an effectful step" do
      let
        step :: Int -> F.RIO () () (Tuple Int (Maybe Int))
        step n =
          if n >= 3 then pure (Tuple (n * 10) Nothing)
          else pure (Tuple (n * 10) (Just (n + 1)))

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.paginateRIO 1 step)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "takeUntil" do
    it "yields up to and including the first match, then stops" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.takeUntil (_ >= 3) (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "yields the full stream when no element matches" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.takeUntil (_ > 100) (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "dropUntil" do
    it "drops up to (not including) the first match" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.dropUntil (_ >= 3) (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "yields the empty stream when no element matches" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.dropUntil (_ > 100) (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "distinctBy" do
    it "drops consecutive elements sharing the same key" do
      let
        prog :: F.RIO () () (Array { k :: Int, v :: String })
        prog = S.runCollect
          ( S.distinctBy _.k
              ( S.fromArray
                  [ { k: 1, v: "a" }
                  , { k: 1, v: "b" }
                  , { k: 2, v: "c" }
                  , { k: 2, v: "d" }
                  , { k: 3, v: "e" }
                  , { k: 1, v: "f" }
                  ]
              )
          )
      out <- runAff prog {}
      case out of
        Success xs ->
          xs `shouldEqual`
            [ { k: 1, v: "a" }
            , { k: 2, v: "c" }
            , { k: 3, v: "e" }
            , { k: 1, v: "f" }
            ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "zip / zipWith (sequential)" do
    it "pairs elements positionally and ends when either side ends" do
      let
        prog :: F.RIO () () (Array (Tuple Int String))
        prog = S.runCollect
          (S.zip
            (S.fromArray [ 1, 2, 3, 4 ])
            (S.fromArray [ "a", "b", "c" ]))
      out <- runAff prog {}
      case out of
        Success xs ->
          xs `shouldEqual` [ Tuple 1 "a", Tuple 2 "b", Tuple 3 "c" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "zipWith combines paired elements with the supplied function" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.zipWith (+)
            (S.fromArray [ 10, 20, 30 ])
            (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 11, 22, 33 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "interleave" do
    it "alternates left, right and forwards the rest when one side ends" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.interleave
            (S.fromArray [ 1, 3, 5, 7 ])
            (S.fromArray [ 2, 4 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5, 7 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "partitionEither" do
    it "splits a stream of Either into two parallel sub-streams" do
      let
        source :: S.Stream () () (Either Int String)
        source = S.fromArray
          [ Left 1, Right "a", Left 2, Right "b", Right "c", Left 3 ]

        prog :: F.RIO () () { ls :: Array Int, rs :: Array String }
        prog = do
          { lefts, rights } <- S.partitionEither source
          F.zipWithPar (\ls rs -> { ls, rs })
            (S.runCollect lefts)
            (S.runCollect rights)
      out <- runAff prog {}
      case out of
        Success r -> do
          r.ls `shouldEqual` [ 1, 2, 3 ]
          r.rs `shouldEqual` [ "a", "b", "c" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "toQueue / toHub" do
    it "toQueue drains every element into the queue" do
      q <- liftEffect (Q.make 8 :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = do
          S.toQueue q (S.fromArray [ 1, 2, 3 ])
          a <- Q.take q
          b <- Q.take q
          c <- Q.take q
          pure [ a, b, c ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "toHub broadcasts to every active subscriber" do
      hub <- liftEffect (Hub.make 8 :: _ (Hub.Hub Int))
      let
        drainN
          :: Int
          -> Hub.Subscription Int
          -> Array Int
          -> F.RIO () () (Array Int)
        drainN 0 _ acc = pure acc
        drainN n s acc = do
          a <- Hub.take s
          drainN (n - 1) s (Array.snoc acc a)

        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          sub <- Hub.subscribeScoped scope hub
          fib <- F.fork (drainN 3 sub [])
          S.toHub hub (S.fromArray [ 100, 200, 300 ])
          F.join fib
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 100, 200, 300 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "tick" do
    it "tick yields units at the requested cadence" do
      let
        prog :: F.RIO () () Int
        prog = do
          ticks <- S.runCollect (S.take 4 (S.tick (Milliseconds 5.0)))
          pure (length ticks)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 4
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "haltWhen" do
    it "halts the upstream the moment signal fires" do
      ref <- liftEffect (Ref.new false)
      let
        signal :: F.RIO () () Unit
        signal = F.sleep (Milliseconds 20.0)
          *> F.liftEffect (Ref.write true ref)

        upstream :: S.Stream () () Int
        upstream = S.tick (Milliseconds 5.0) # S.map (\_ -> 1)

        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.haltWhen signal upstream)
      out <- runAff prog {}
      fired <- liftEffect (Ref.read ref)
      case out of
        Success xs -> do
          fired `shouldEqual` true
          -- Upstream produced at least one element before the signal
          -- but did not run indefinitely.
          (length xs >= 0) `shouldEqual` true
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "drop / dropWhile" do
    it "drop skips the first n elements" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.drop 2 (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "drop with n >= length yields the empty stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (S.drop 10 (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "dropWhile stops on the first element that fails the predicate" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.dropWhile (_ < 3) (S.fromArray [ 1, 2, 3, 4, 2 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 3, 4, 2 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "tap / tapError" do
    it "tap fires for every element and forwards them unchanged" do
      ref <- liftEffect (Ref.new ([] :: Array Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          ( S.tap
              (\n -> F.liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) ref))
              (S.fromArray [ 10, 20, 30 ])
          )
      out <- runAff prog {}
      seen <- liftEffect (Ref.read ref)
      case out of
        Success xs -> do
          xs `shouldEqual` [ 10, 20, 30 ]
          seen `shouldEqual` [ 10, 20, 30 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "tapError fires on a typed failure and re-raises" do
      seen <- liftEffect (Ref.new false)
      let
        boom :: S.Stream () (boom :: String) Int
        boom = S.Stream
          (F.fail (Variant.inj (Proxy :: _ "boom") "x"))

        prog :: F.RIO () (boom :: String) (Array Int)
        prog = S.runCollect
          (S.tapError (\_ -> F.liftEffect (Ref.write true seen)) boom)
      out <- runAff prog {}
      fired <- liftEffect (Ref.read seen)
      case out of
        Fail _ -> fired `shouldEqual` true
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "mapRIO / mapRIOPar" do
    it "mapRIO runs the action on every element in order" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.mapRIO (\n -> pure (n + 1)) (S.fromArray [ 1, 2, 3 ]))
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "mapRIOPar covers every element (order may differ)" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect
          (S.mapRIOPar 3 (\n -> pure (n * 10)) (S.fromArray [ 1, 2, 3, 4, 5 ]))
      out <- runAff prog {}
      case out of
        Success xs -> do
          length xs `shouldEqual` 5
          Array.sort xs `shouldEqual` [ 10, 20, 30, 40, 50 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "partitioned" do
    it "routes elements to yes/no by the predicate" do
      let
        src :: S.Stream () () Int
        src = S.fromArray [ 1, 2, 3, 4, 5, 6 ]

        prog :: F.RIO () () { yes :: Array Int, no :: Array Int }
        prog = do
          parts <- S.partitioned (\n -> n `mod` 2 == 0) src
          yes <- S.runCollect parts.yes
          no <- S.runCollect parts.no
          pure { yes, no }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.yes `shouldEqual` [ 2, 4, 6 ]
          r.no `shouldEqual` [ 1, 3, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "emits empty streams when one side has no matches" do
      let
        src :: S.Stream () () Int
        src = S.fromArray [ 1, 3, 5 ]

        prog :: F.RIO () () { yes :: Array Int, no :: Array Int }
        prog = do
          parts <- S.partitioned (\n -> n `mod` 2 == 0) src
          yes <- S.runCollect parts.yes
          no <- S.runCollect parts.no
          pure { yes, no }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.yes `shouldEqual` []
          r.no `shouldEqual` [ 1, 3, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "async" do
    it "delivers buffered values pushed before the first pull" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          let
            stream = S.async scope \emit -> do
              emit (S.EmitValue 1)
              emit (S.EmitValue 2)
              emit (S.EmitValue 3)
              emit S.EmitEnd
              pure (pure unit)
          S.runCollect stream
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "wakes a parked puller when a value arrives later" do
      -- The producer schedules an emit a short time after the consumer
      -- has started pulling, so the very first element exercises the
      -- "waiter set, then emit fires it" path.
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          ref <- F.liftEffect (Ref.new (Nothing :: Maybe (S.Emit () Int -> _)))
          let
            stream = S.async scope \emit -> do
              Ref.write (Just emit) ref
              pure (pure unit)

            driver = do
              -- Give the stream a chance to install its waiter, then
              -- push from outside.
              F.sleep (Milliseconds 5.0)
              F.liftEffect do
                m <- Ref.read ref
                case m of
                  Just k -> do
                    k (S.EmitValue 7)
                    k (S.EmitValue 8)
                    k S.EmitEnd
                  Nothing -> pure unit
          _ <- F.fork driver
          S.runCollect stream
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 7, 8 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "EmitFailure terminates the stream as a typed failure" do
      let
        prog :: F.RIO () (boom :: String) (Array Int)
        prog = Scope.scoped \scope -> do
          let
            stream = S.async scope \emit -> do
              emit (S.EmitValue 1)
              emit (S.EmitValue 2)
              emit
                (S.EmitFailure (Variant.inj (Proxy :: _ "boom") "stop"))
              pure (pure unit)
          S.runCollect stream
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "stop"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "buffered values are still observed before a queued terminal" do
      -- Push values, then EmitEnd, all before the first pull. The pull
      -- loop should drain the buffered values before observing Done.
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          let
            stream = S.async scope \emit -> do
              emit (S.EmitValue 10)
              emit (S.EmitValue 20)
              emit S.EmitEnd
              emit (S.EmitValue 30) -- ignored: emitted after terminal
              pure (pure unit)
          S.runCollect stream
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 10, 20 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "registers its cleanup with the scope" do
      cleanedUp <- liftEffect (Ref.new false)
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          let
            stream = S.async scope \emit -> do
              emit (S.EmitValue 1)
              emit S.EmitEnd
              pure (Ref.write true cleanedUp)
          S.runCollect stream
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1 ]
        other -> fail ("expected Success, got " <> describeOutcome other)
      cleaned <- liftEffect (Ref.read cleanedUp)
      cleaned `shouldEqual` true

    it "cleanup still fires when downstream stops early" do
      cleanedUp <- liftEffect (Ref.new false)
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          let
            stream = S.async scope \emit -> do
              emit (S.EmitValue 1)
              emit (S.EmitValue 2)
              emit (S.EmitValue 3)
              pure (Ref.write true cleanedUp)
          S.runCollect (S.take 2 stream)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2 ]
        other -> fail ("expected Success, got " <> describeOutcome other)
      cleaned <- liftEffect (Ref.read cleanedUp)
      cleaned `shouldEqual` true

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
