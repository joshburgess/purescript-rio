module Test.RIO.Node.ChildProcessSpec (spec) where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (once) as NE
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Core (RIO, runRIO')
import RIO.Node.Buffer as Buf
import RIO.Node.ChildProcess
  ( ChildProcess
  , Exit(..)
  , exitH
  ) as Exports
import RIO.Node.ChildProcess as CP

runCP :: forall a. RIO () () a -> Aff a
runCP = runRIO'

-- Block until the child fires `exit`.
awaitExit :: Exports.ChildProcess -> Aff Exports.Exit
awaitExit cp = makeAff \done -> do
  remove <- cp # NE.once Exports.exitH \e -> done (Right e)
  pure (effectCanceler remove)

spec :: Spec Unit
spec = describe "RIO.Node.ChildProcess" do
  it "spawnSync captures stdout from the child" do
    out <- runCP do
      r <- CP.spawnSync "node" [ "-e", "process.stdout.write('hello')" ]
      Buf.toString UTF8 r.stdout
    out `shouldEqual` "hello"

  it "spawnSync exits Normally 0 for a successful command" do
    status <- runCP do
      r <- CP.spawnSync "node" [ "-e", "process.exit(0)" ]
      pure r.exitStatus
    case status of
      Exports.Normally code -> code `shouldEqual` 0
      _ -> 0 `shouldEqual` 1

  it "spawnSync exits Normally 7 for an explicit exit code" do
    status <- runCP do
      r <- CP.spawnSync "node" [ "-e", "process.exit(7)" ]
      pure r.exitStatus
    case status of
      Exports.Normally code -> code `shouldEqual` 7
      _ -> 0 `shouldEqual` 1

  it "waitSpawned returns Right Pid for a valid executable" do
    r <- runCP do
      cp <- CP.spawn "node" [ "-e", "" ]
      CP.waitSpawned cp
    isRight r `shouldEqual` true

  it "spawn + exitH fires with Normally 0 on clean exit" do
    e <- do
      cp <- runCP (CP.spawn "node" [ "-e", "process.exit(0)" ])
      awaitExit cp
    case e of
      Exports.Normally code -> code `shouldEqual` 0
      _ -> 0 `shouldEqual` 1

  it "pid is Just after the child has spawned" do
    out <- do
      cp <- runCP (CP.spawn "node" [ "-e", "" ])
      _ <- awaitExit cp
      runCP (CP.pid cp)
    isJust out `shouldEqual` true

  it "exitCode is Just 0 once a clean exit has fired" do
    out <- do
      cp <- runCP (CP.spawn "node" [ "-e", "process.exit(0)" ])
      _ <- awaitExit cp
      runCP (CP.exitCode cp)
    out `shouldEqual` Just 0

  it "kill on a long-running child terminates it and flips `killed`" do
    out <- do
      cp <- runCP
        (CP.spawn "node" [ "-e", "setInterval(() => {}, 1000)" ])
      _ <- runCP (CP.waitSpawned cp)
      didKill <- runCP (CP.kill cp)
      _ <- awaitExit cp
      flagged <- runCP (CP.killed cp)
      pure { didKill, flagged }
    out.didKill `shouldEqual` true
    out.flagged `shouldEqual` true

  it "exec runs a shell command and fires exitH" do
    e <- do
      cp <- runCP (CP.exec "exit 0")
      awaitExit cp
    case e of
      Exports.Normally code -> code `shouldEqual` 0
      _ -> 0 `shouldEqual` 1

  it "execSync returns the command's stdout as a Buffer" do
    out <- runCP do
      buf <- CP.execSync "printf hi"
      Buf.toString UTF8 buf
    out `shouldSatisfy` (\s -> s == "hi")
