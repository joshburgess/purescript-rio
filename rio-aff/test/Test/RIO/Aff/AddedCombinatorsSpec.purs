module Test.RIO.Aff.AddedCombinatorsSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Array ((..))
import Data.Array.NonEmpty (length) as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable) as Set
import Data.String (length) as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause (Cause(..), ensuringWith, onExit, prettyPrint)
import RIO.Aff.Clock (Clock)
import RIO.Aff.Concurrency (fork, join, loop, zipFiber)
import RIO.Aff.Core (RIO, provideAll, runRIO, runRIO')
import RIO.Aff.Deferred (isDoneDeferred, makeDeferred, succeedDeferred)
import RIO.Aff.Error (filterOrDie, filterOrFail, firstSuccessOf, rethrow)
import RIO.Aff.Hub (Hub)
import RIO.Aff.Hub as Hub
import RIO.Aff.Layer (Layer, buildLayer, fromRecord, mergeLayers)
import RIO.Aff.Metric.Counter as Counter
import RIO.Aff.Metric.Histogram as Histogram
import RIO.Aff.Queue as Queue
import RIO.Aff.Random (bytes, uuid)
import RIO.Aff.Schedule (Schedule, Step(..), bothS, compose, delays, mapOutput, passthrough, recurs, step)
import RIO.Aff.Schedule as Schedule
import RIO.Aff.Semaphore (make, parTraverseN, validateParN) as Semaphore
import RIO.Aff.STM (atomically, newTVar, readTVar, swapTVar)
import RIO.Aff.STM.TArray as TArray
import RIO.Aff.STM.TMap as TMap
import RIO.Aff.STM.TSemaphore as TSemaphore
import RIO.Aff.Sink (Sink, dropN, dropWhile, findM, mkString, runSink, takeN)
import RIO.Aff.Sink as Sink
import RIO.Aff.Stream (Stream, concatAll, forEach, fromArray, runCollect)
import RIO.Aff.Stream as Stream
import RIO.Aff.Stream.Par (mapPar, mapRIOPar, zipLatest, zipPar) as Par
import RIO.Aff.Test.Clock (newTestClock)

type E = (oops :: String)

oops :: String -> Variant E
oops = Variant.inj (Proxy :: Proxy "oops")

spec :: Spec Unit
spec = do
  describe "RIO.Aff (added combinators)" do

    describe "Error.filterOrFail" do
      it "passes when the predicate holds" do
        r <- runRIO
          ( filterOrFail (_ > 0) (\_ -> oops "neg") (pure 1)
              :: RIO () E Int
          )
        r `shouldEqual` Right 1

      it "raises a typed failure when the predicate fails" do
        r <- runRIO
          ( filterOrFail (_ > 0) (\n -> oops (show n)) (pure 0)
              :: RIO () E Int
          )
        r `shouldEqual` Left (oops "0")

    describe "Error.filterOrDie" do
      it "passes when the predicate holds" do
        r <- runRIO'
          ( filterOrDie (_ > 0) (\_ -> error "neg") (pure 1)
              :: RIO () () Int
          )
        r `shouldEqual` 1

    describe "Error.firstSuccessOf" do
      it "returns the first success" do
        let
          actions :: Array (RIO () E Int)
          actions =
            [ rethrow (oops "a")
            , pure 42
            , rethrow (oops "b")
            ]
        r <- runRIO (firstSuccessOf actions)
        r `shouldEqual` Right 42

      it "raises the last failure when every action fails" do
        let
          actions :: Array (RIO () E Int)
          actions =
            [ rethrow (oops "a")
            , rethrow (oops "b")
            , rethrow (oops "c")
            ]
        r <- runRIO (firstSuccessOf actions)
        r `shouldEqual` Left (oops "c")

    describe "Concurrency.loop" do
      it "iterates a stateful condition and collects results" do
        r <- runRIO'
          ( loop 0 (\s -> s < 4) (_ + 1) (\s -> pure (s * 10))
              :: RIO () () (Array Int)
          )
        r `shouldEqual` [ 0, 10, 20, 30 ]

    describe "Concurrency.zipFiber" do
      it "joins two fibers into one pair" do
        zipped <- runRIO' do
          f1 <- fork (pure 1 :: RIO () () Int)
          f2 <- fork (pure "x" :: RIO () () String)
          zipFiber f1 f2
        result <- runRIO (join zipped)
        result `shouldEqual` Right (Tuple 1 "x")

    describe "Cause.ensuringWith" do
      it "runs the finalizer on success" do
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          record :: String -> RIO () () Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

          program :: RIO () () Int
          program = ensuringWith (pure 7) \result ->
            case result of
              Right _ -> record "ok"
              Left _ -> record "fail"

        n <- runRIO' program
        n `shouldEqual` 7
        recorded <- liftEffect (Ref.read log)
        recorded `shouldEqual` [ "ok" ]

      it "runs the finalizer on a typed failure" do
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          record :: String -> RIO () E Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

          program :: RIO () E Int
          program = ensuringWith (rethrow (oops "boom")) \result ->
            case result of
              Right _ -> record "ok"
              Left _ -> record "fail"

        r <- runRIO program
        r `shouldEqual` Left (oops "boom")
        recorded <- liftEffect (Ref.read log)
        recorded `shouldEqual` [ "fail" ]

    describe "Cause.onExit" do
      it "does not run for a success" do
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          record :: String -> RIO () () Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

          program :: RIO () () Int
          program = onExit (pure 1) (\_ -> record "exit")

        _ <- runRIO' program
        recorded <- liftEffect (Ref.read log)
        recorded `shouldEqual` []

      it "fires on a typed failure" do
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          record :: String -> RIO () () Unit
          record s =
            liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

          program :: RIO () E Int
          program =
            onExit (rethrow (oops "x")) (\_ -> record "exit")

        _ <- runRIO program
        recorded <- liftEffect (Ref.read log)
        recorded `shouldEqual` [ "exit" ]

    describe "Schedule.passthrough" do
      it "yields its input unchanged with zero delay" do
        let s = passthrough :: Schedule () Int Int
        r <- runRIO' (step s 5)
        case r of
          Continue out d _ -> do
            out `shouldEqual` 5
            d `shouldEqual` Milliseconds 0.0
          Done -> shouldEqual true false

    describe "Schedule.compose" do
      it "produces Done when either side is done" do
        -- recurs 0 is immediately Done; compose with passthrough
        -- should also be Done.
        let s = compose (recurs 0 :: Schedule () Int Int) passthrough
        r <- runRIO' (step s 5)
        case r of
          Done -> pure unit
          Continue _ _ _ -> shouldEqual true false

    describe "Schedule.delays" do
      it "wraps a schedule that's still going" do
        let s = delays (recurs 3 :: Schedule () Int Int)
        r <- runRIO' (step s 0)
        case r of
          Continue d _ _ -> d `shouldEqual` Milliseconds 0.0
          Done -> shouldEqual true false

    describe "Semaphore.parTraverseN" do
      it "delivers every result" do
        sem <- liftEffect (Semaphore.make 2)
        r <- runRIO'
          ( Semaphore.parTraverseN sem (\n -> pure (n * 10))
              [ 1, 2, 3, 4, 5 ]
              :: RIO () () (Array Int)
          )
        r `shouldEqual` [ 10, 20, 30, 40, 50 ]

    describe "Sink combinators" do
      it "dropN skips the first N inputs then collects the rest" do
        r <- runRIO
          ( runSink
              (dropN 2 :: Sink () () Int (Array Int))
              (fromArray [ 1, 2, 3, 4, 5 ])
          )
        r `shouldEqual` Right [ 3, 4, 5 ]

      it "dropWhile skips the leading prefix that matches" do
        r <- runRIO
          ( runSink
              (dropWhile (_ < 3) :: Sink () () Int (Array Int))
              (fromArray [ 1, 2, 3, 4, 1 ])
          )
        r `shouldEqual` Right [ 3, 4, 1 ]

      it "findM returns the first input matching an effectful predicate" do
        r <- runRIO
          ( runSink
              (findM (\n -> pure (n > 2)) :: Sink () () Int _)
              (fromArray [ 1, 2, 3, 4 ])
          )
        r `shouldEqual` Right (Just 3)

      it "mkString joins inputs with a separator" do
        r <- runRIO
          ( runSink
              (mkString ", " :: Sink () () String String)
              (fromArray [ "a", "b", "c" ])
          )
        r `shouldEqual` Right "a, b, c"

    describe "STM.swapTVar" do
      it "returns the previous value and replaces it" do
        let
          program :: RIO () () (Tuple Int Int)
          program = do
            v <- atomically (newTVar 1)
            old <- atomically (swapTVar v 99)
            cur <- atomically (readTVar v)
            pure (Tuple old cur)
        r <- runRIO' program
        r `shouldEqual` Tuple 1 99

    describe "STM.TMap.modifyTMap" do
      it "writes back Just and deletes on Nothing" do
        let
          program :: RIO () () (Tuple (Maybe Int) (Maybe Int))
          program = do
            m <- atomically TMap.newTMap
            atomically (TMap.insertTMap "k" 1 m)
            atomically (TMap.modifyTMap "k" (\n -> Just (n + 10)) m)
            after1 <- atomically (TMap.lookupTMap "k" m)
            atomically (TMap.modifyTMap "k" (\_ -> Nothing) m)
            after2 <- atomically (TMap.lookupTMap "k" m)
            pure (Tuple after1 after2)
        r <- runRIO' program
        r `shouldEqual` Tuple (Just 11) Nothing

    describe "STM.TSemaphore.tryAcquireN" do
      it "deducts permits when available, returns false otherwise" do
        let
          program :: RIO () () (Tuple Boolean (Tuple Boolean Int))
          program = do
            sem <- atomically (TSemaphore.newTSemaphore 2)
            ok1 <- atomically (TSemaphore.tryAcquireTSemaphore sem)
            ok2 <- atomically (TSemaphore.tryAcquireN 2 sem)
            left <- atomically (TSemaphore.availableTSemaphore sem)
            pure (Tuple ok1 (Tuple ok2 left))
        r <- runRIO' program
        r `shouldEqual` Tuple true (Tuple false 1)

    describe "STM.TArray.replicateTArray" do
      it "runs the STM action n times to fill the array" do
        let
          program :: RIO () () (Array Int)
          program = atomically do
            counter <- newTVar 0
            arr <- TArray.replicateTArray 3 do
              c <- readTVar counter
              _ <- swapTVar counter (c + 1)
              pure c
            TArray.toArrayTArray arr
        r <- runRIO' program
        r `shouldEqual` [ 0, 1, 2 ]

    describe "Hub.subscribers" do
      it "reports the live subscriber count" do
        hub <- liftEffect (Hub.make :: _ (Hub Int))
        n0 <- runRIO' (Hub.subscribers hub :: RIO () () Int)
        n0 `shouldEqual` 0
        sub <- runRIO' (Hub.subscribe hub :: RIO () () _)
        n1 <- runRIO' (Hub.subscribers hub :: RIO () () Int)
        n1 `shouldEqual` 1
        runRIO' sub.unsubscribe
        n2 <- runRIO' (Hub.subscribers hub :: RIO () () Int)
        n2 `shouldEqual` 0

    describe "Deferred.isDoneDeferred" do
      it "returns false before the cell is filled and true after" do
        let
          program :: RIO () () (Tuple Boolean Boolean)
          program = do
            d <- makeDeferred
            before <- isDoneDeferred d
            _ <- succeedDeferred d 1
            after <- isDoneDeferred d
            pure (Tuple before after)
        r <- runRIO' program
        r `shouldEqual` Tuple false true

    describe "Test.Clock.setEpoch / readEpoch" do
      it "jumps virtual time to an absolute value" do
        tc <- liftAff newTestClock
        epoch0 <- liftAff tc.readEpoch
        epoch0 `shouldEqual` Milliseconds 0.0
        liftAff (tc.setEpoch (Milliseconds 500.0))
        epoch1 <- liftAff tc.readEpoch
        epoch1 `shouldEqual` Milliseconds 500.0

    describe "Stream.Par.mapPar" do
      it "preserves upstream order under concurrent worker execution" do
        r <- runRIO
          ( runCollect
              ( Par.mapPar 3
                  (\n -> pure (n * 10))
                  (fromArray (1 .. 10))
              ) :: RIO () () _
          )
        case r of
          Right xs -> xs `shouldEqual` map (_ * 10) (1 .. 10)
          Left _ -> shouldEqual true false

    describe "Stream.Par.zipPar" do
      it "pairs elements and ends with the shorter side" do
        let
          sa = fromArray [ 1, 2, 3, 4 ] :: Stream () () Int
          sb = fromArray [ "a", "b" ] :: Stream () () String
        r <- runRIO (runCollect (Par.zipPar sa sb))
        case r of
          Right xs ->
            xs `shouldEqual` [ Tuple 1 "a", Tuple 2 "b" ]
          Left _ -> shouldEqual true false

    describe "Stream.Par.zipLatest" do
      it "produces at least one pair when both sides yield" do
        let
          sa = fromArray [ 1, 2, 3 ] :: Stream () () Int
          sb = fromArray [ "x", "y" ] :: Stream () () String
        r <- runRIO (runCollect (Par.zipLatest sa sb))
        case r of
          Right xs ->
            (Array.length xs >= 1) `shouldEqual` true
          Left _ -> shouldEqual true false

    describe "Cause.prettyPrint" do
      it "renders a typed-failure leaf using the render function" do
        let
          c :: Cause E
          c = Fail (oops "boom")
          rendered = prettyPrint (\_ -> "OOPS") c
        rendered `shouldEqual` "Fail OOPS"

    describe "Sink.takeN / mkSink" do
      it "takeN is an alias for take" do
        r <- runRIO
          ( runSink
              (takeN 3 :: Sink () () Int (Array Int))
              (fromArray [ 1, 2, 3, 4, 5 ])
          )
        r `shouldEqual` Right [ 1, 2, 3 ]

      it "mkSink wraps an RIO step into a Sink" do
        let
          oneStep :: Sink () () Int (Maybe Int)
          oneStep = Sink.mkSink
            ( pure
                ( Sink.Need
                    (\i -> Sink.mkSink (pure (Sink.Halt (Just i))))
                    (pure Nothing)
                )
            )
        r <- runRIO (runSink oneStep (fromArray [ 7, 8, 9 ]))
        r `shouldEqual` Right (Just 7)

    describe "Schedule.bothS / mapOutput" do
      it "bothS pairs outputs of two schedules" do
        let s = bothS (passthrough :: Schedule () Int Int) passthrough
        r <- runRIO' (step s 5)
        case r of
          Continue out _ _ -> out `shouldEqual` Tuple 5 5
          Done -> shouldEqual true false

      it "mapOutput rewrites the schedule's output" do
        let
          s :: Schedule () Int String
          s = mapOutput show (passthrough :: Schedule () Int Int)
        r <- runRIO' (step s 42)
        case r of
          Continue out _ _ -> out `shouldEqual` "42"
          Done -> shouldEqual true false

    describe "Semaphore.validateParN" do
      it "accumulates every typed failure when any branch fails" do
        sem <- liftEffect (Semaphore.make 2)
        let
          worker :: Int -> RIO () E Int
          worker n
            | n `mod` 2 == 0 = pure n
            | otherwise = rethrow (oops (show n))
        r <- runRIO'
          ( Semaphore.validateParN sem worker [ 1, 2, 3, 4 ]
              :: RIO () () (Either _ (Array Int))
          )
        case r of
          Left errs -> NEA.length errs `shouldEqual` 2
          Right _ -> shouldEqual true false

      it "returns Right of all results when every branch succeeds" do
        sem <- liftEffect (Semaphore.make 2)
        let
          worker :: Int -> RIO () E Int
          worker n = pure (n * 10)
        r <- runRIO'
          ( Semaphore.validateParN sem worker [ 1, 2, 3 ]
              :: RIO () () (Either _ (Array Int))
          )
        case r of
          Right xs -> xs `shouldEqual` [ 10, 20, 30 ]
          Left _ -> shouldEqual true false

    describe "Stream.forEach / concatAll" do
      it "forEach runs the action on each element" do
        ref <- liftEffect (Ref.new ([] :: Array Int))
        _ <- runRIO'
          ( forEach
              (\n -> liftEffect (Ref.modify_ (_ <> [ n ]) ref))
              (fromArray [ 1, 2, 3 ])
              :: RIO () () Unit
          )
        seen <- liftEffect (Ref.read ref)
        seen `shouldEqual` [ 1, 2, 3 ]

      it "concatAll flattens an array of streams in order" do
        let
          parts :: Array (Stream () () Int)
          parts = [ fromArray [ 1, 2 ], fromArray [ 3 ], fromArray [ 4, 5 ] ]
        r <- runRIO (runCollect (concatAll parts))
        r `shouldEqual` Right [ 1, 2, 3, 4, 5 ]

    describe "Queue.dropping / tryOffer / tryTake" do
      it "dropping drops new values once full and tryOffer reports the drop" do
        q <- liftEffect (Queue.dropping 2 :: _ (Queue.Queue Int))
        ok1 <- runRIO' (Queue.tryOffer q 1 :: RIO () () Boolean)
        ok2 <- runRIO' (Queue.tryOffer q 2 :: RIO () () Boolean)
        ok3 <- runRIO' (Queue.tryOffer q 3 :: RIO () () Boolean)
        ok1 `shouldEqual` true
        ok2 `shouldEqual` true
        ok3 `shouldEqual` false
        v1 <- runRIO' (Queue.tryTake q :: RIO () () (Maybe Int))
        v2 <- runRIO' (Queue.tryTake q :: RIO () () (Maybe Int))
        v3 <- runRIO' (Queue.tryTake q :: RIO () () (Maybe Int))
        v1 `shouldEqual` Just 1
        v2 `shouldEqual` Just 2
        v3 `shouldEqual` Nothing

    describe "Hub.tryPublish / publishDropNew / publishDropOld" do
      it "tryPublish is always true on a default unbounded hub" do
        hub <- liftEffect (Hub.make :: _ (Hub Int))
        _ <- runRIO' (Hub.subscribe hub :: RIO () () _)
        ok <- runRIO' (Hub.tryPublish hub 1 :: RIO () () Boolean)
        ok `shouldEqual` true

      it "publishDropNew drops values for full subscriber queues" do
        hub <- liftEffect (Hub.makeBounded 1 :: _ (Hub Int))
        sub <- runRIO' (Hub.subscribe hub :: RIO () () _)
        runRIO' (Hub.publishDropNew hub 1 :: RIO () () Unit)
        runRIO' (Hub.publishDropNew hub 2 :: RIO () () Unit)
        v1 <- runRIO' (Queue.tryTake sub.queue :: RIO () () (Maybe Int))
        v2 <- runRIO' (Queue.tryTake sub.queue :: RIO () () (Maybe Int))
        v1 `shouldEqual` Just 1
        v2 `shouldEqual` Nothing

      it "publishDropOld evicts the oldest element when the queue is full" do
        hub <- liftEffect (Hub.makeBounded 1 :: _ (Hub Int))
        sub <- runRIO' (Hub.subscribe hub :: RIO () () _)
        runRIO' (Hub.publishDropOld hub 1 :: RIO () () Unit)
        runRIO' (Hub.publishDropOld hub 2 :: RIO () () Unit)
        v1 <- runRIO' (Queue.tryTake sub.queue :: RIO () () (Maybe Int))
        v2 <- runRIO' (Queue.tryTake sub.queue :: RIO () () (Maybe Int))
        v1 `shouldEqual` Just 2
        v2 `shouldEqual` Nothing

    describe "Stream.buffer / bufferDropping / bufferSliding" do
      it "buffer preserves every element in order" do
        r <- runRIO
          ( runCollect
              (Stream.buffer 4 (fromArray [ 1, 2, 3, 4, 5 ]))
              :: RIO () () _
          )
        case r of
          Right xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
          Left _ -> shouldEqual true false

      it "bufferDropping yields a prefix when capacity is small" do
        r <- runRIO
          ( runCollect
              (Stream.bufferDropping 1 (fromArray [ 1, 2, 3, 4, 5 ]))
              :: RIO () () _
          )
        case r of
          Right xs ->
            (Array.length xs >= 1 && Array.length xs <= 5)
              `shouldEqual` true
          Left _ -> shouldEqual true false

      it "bufferSliding yields a suffix-leaning subsequence" do
        r <- runRIO
          ( runCollect
              (Stream.bufferSliding 1 (fromArray [ 1, 2, 3, 4, 5 ]))
              :: RIO () () _
          )
        case r of
          Right xs ->
            (Array.length xs >= 1 && Array.length xs <= 5)
              `shouldEqual` true
          Left _ -> shouldEqual true false

    describe "Layer.mergeLayers" do
      it "merges two layers' outputs into one record" do
        let
          la :: Layer () () (a :: Int)
          la = fromRecord { a: 1 }
          lb :: Layer () () (b :: String)
          lb = fromRecord { b: "x" }
          merged :: Layer () () (a :: Int, b :: String)
          merged = mergeLayers la lb
        r <- buildLayer merged
        case r of
          Right rec -> do
            rec.a `shouldEqual` 1
            rec.b `shouldEqual` "x"
          Left _ -> shouldEqual true false

    describe "Stream.unchunked / mapChunks / chunked" do
      it "unchunked flattens chunks back to elements" do
        r <- runRIO
          ( runCollect
              ( Stream.unchunked
                  (fromArray [ [ 1, 2 ], [ 3 ], [ 4, 5 ] ])
              ) :: RIO () () _
          )
        r `shouldEqual` Right [ 1, 2, 3, 4, 5 ]

      it "mapChunks applies a function per chunk" do
        r <- runRIO
          ( runCollect
              ( Stream.mapChunks (map (_ * 10))
                  (fromArray [ [ 1, 2 ], [ 3 ] ])
              ) :: RIO () () _
          )
        r `shouldEqual` Right [ [ 10, 20 ], [ 30 ] ]

      it "chunked is an alias for chunk" do
        r <- runRIO
          ( runCollect
              (Stream.chunked 2 (fromArray [ 1, 2, 3, 4, 5 ]))
              :: RIO () () _
          )
        r `shouldEqual` Right [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]

    describe "Stream.catchAll" do
      it "recovers from a typed failure with a fallback stream" do
        let
          source :: Stream () E Int
          source = Stream.concat
            (fromArray [ 1, 2 ])
            (Stream.Stream (rethrow (oops "boom")))
          recovered :: Stream () () Int
          recovered =
            Stream.catchAll (\_ -> fromArray [ 99 ]) source
        r <- runRIO (runCollect recovered)
        r `shouldEqual` Right [ 1, 2, 99 ]

      it "passes through a successful stream unchanged" do
        let
          source :: Stream () E Int
          source = fromArray [ 1, 2, 3 ]
          recovered :: Stream () () Int
          recovered =
            Stream.catchAll (\_ -> fromArray [ 99 ]) source
        r <- runRIO (runCollect recovered)
        r `shouldEqual` Right [ 1, 2, 3 ]

    describe "Stream.retry" do
      it "retries a failing pull up to the schedule's budget" do
        attempts <- liftEffect (Ref.new 0)
        let
          flaky :: Stream (clock :: Clock) E Int
          flaky = Stream.Stream do
            n <- liftEffect (Ref.modify (_ + 1) attempts)
            if n < 3 then rethrow (oops "still warming up")
            else pure (Stream.Yield 42 Stream.empty)
          sched :: Schedule (clock :: Clock) (Variant E) Int
          sched = recurs 5
        tc <- liftAff newTestClock
        let env = { clock: tc.clock }
        r <- runRIO
          ( provideAll env (runCollect (Stream.retry sched flaky))
              :: RIO () E _
          )
        r `shouldEqual` Right [ 42 ]

      it "re-raises the last failure when the budget is exhausted" do
        attempts <- liftEffect (Ref.new 0)
        let
          alwaysFails :: Stream (clock :: Clock) E Int
          alwaysFails = Stream.Stream do
            _ <- liftEffect (Ref.modify (_ + 1) attempts)
            rethrow (oops "nope")
          sched :: Schedule (clock :: Clock) (Variant E) Int
          sched = recurs 2
        tc <- liftAff newTestClock
        let env = { clock: tc.clock }
        r <- runRIO
          ( provideAll env
              (runCollect (Stream.retry sched alwaysFails))
              :: RIO () E _
          )
        r `shouldEqual` Left (oops "nope")
        n <- liftEffect (Ref.read attempts)
        n `shouldEqual` 3

    describe "Stream.Par.mapRIOPar" do
      it "delivers every element (order-insensitive)" do
        r <- runRIO
          ( runCollect
              ( Par.mapRIOPar 3
                  (\n -> pure (n * 10))
                  (fromArray (1 .. 10))
              ) :: RIO () () _
          )
        case r of
          Right xs ->
            (Set.fromFoldable xs :: _ Int)
              `shouldEqual`
                (Set.fromFoldable (map (_ * 10) (1 .. 10)))
          Left _ -> shouldEqual true false

      it "propagates a typed failure from a worker" do
        let
          worker :: Int -> RIO () E Int
          worker n
            | n == 3 = rethrow (oops "boom")
            | otherwise = pure n
        r <- runRIO
          ( runCollect
              (Par.mapRIOPar 2 worker (fromArray (1 .. 5)))
              :: RIO () E _
          )
        case r of
          Left v -> v `shouldEqual` oops "boom"
          Right _ -> shouldEqual true false

    describe "Random.uuid / bytes" do
      it "uuid returns a canonical 36-char hyphenated string" do
        s <- runRIO' (uuid :: RIO () () String)
        String.length s `shouldEqual` 36

      it "bytes returns exactly n entries (n > 0)" do
        xs <- runRIO' (bytes 16 :: RIO () () (Array Int))
        Array.length xs `shouldEqual` 16

      it "bytes returns an empty array for n <= 0" do
        xs <- runRIO' (bytes 0 :: RIO () () (Array Int))
        Array.length xs `shouldEqual` 0

    describe "Metric.Counter.withCounter" do
      it "increments the counter once on success" do
        c <- runRIO' (Counter.make :: RIO () () Counter.Counter)
        r <- runRIO'
          ( Counter.withCounter c (pure 7 :: RIO () () Int)
          )
        r `shouldEqual` 7
        n <- runRIO' (Counter.value c :: RIO () () Number)
        n `shouldEqual` 1.0

      it "increments the counter once on typed failure" do
        c <- runRIO' (Counter.make :: RIO () () Counter.Counter)
        r <- runRIO
          ( Counter.withCounter c
              (rethrow (oops "boom") :: RIO () E Int)
          )
        r `shouldEqual` Left (oops "boom")
        n <- runRIO' (Counter.value c :: RIO () () Number)
        n `shouldEqual` 1.0

    describe "Metric.Histogram.withTimer" do
      it "records an observation regardless of outcome" do
        tc <- liftAff newTestClock
        let env = { clock: tc.clock }
        h <- runRIO'
          ( Histogram.make [ 100.0, 500.0 ]
              :: RIO () () Histogram.Histogram
          )
        _ <- runRIO'
          ( provideAll env (Histogram.withTimer h (pure 1 :: _ Int))
              :: RIO () () Int
          )
        snap <- runRIO' (Histogram.snapshot h :: RIO () () _)
        snap.count `shouldEqual` 1
