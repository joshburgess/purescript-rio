module Test.RIO.Aff.Node.ProcessSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..), isJust)
import Data.String (length) as String
import Effect.Aff (Aff)
import Foreign.Object (member, size) as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Node.Process
  ( Process
  , argv
  , cwd
  , execPath
  , getEnv
  , liveProcess
  , lookupEnv
  , memoryUsageRss
  , pid
  , platformStr
  , ppid
  , setEnv
  , unsetEnv
  , uptime
  , version
  )

runProc :: forall a. RIO (process :: Process) () a -> Aff a
runProc p = runRIO' (provideAll { process: liveProcess } p)

-- A distinctive variable name unlikely to collide with the host
-- shell environment.
varName :: String
varName = "RIO_NODE_PROCESS_SPEC_FIXTURE"

spec :: Spec Unit
spec = describe "RIO.Aff.Node.Process (live)" do
  it "pid renders as a non-empty integer" do
    String.length (show pid) `shouldSatisfy` (_ > 0)

  it "ppid is distinct from pid (running under a real parent)" do
    (show pid /= show ppid) `shouldEqual` true

  it "platformStr is non-empty" do
    String.length platformStr `shouldSatisfy` (_ > 0)

  it "version is non-empty" do
    String.length version `shouldSatisfy` (_ > 0)

  it "cwd returns a non-empty path" do
    c <- runProc cwd
    String.length c `shouldSatisfy` (_ > 0)

  it "argv returns the array of command-line args" do
    a <- runProc argv
    (Array.length a > 0) `shouldEqual` true

  it "execPath returns a non-empty path" do
    p <- runProc execPath
    String.length p `shouldSatisfy` (_ > 0)

  it "uptime is non-negative" do
    u <- runProc uptime
    (u >= 0.0) `shouldEqual` true

  it "memoryUsageRss returns a positive byte count" do
    m <- runProc memoryUsageRss
    (m > 0) `shouldEqual` true

  it "getEnv returns a non-empty environment map" do
    env <- runProc getEnv
    Object.size env `shouldSatisfy` (_ > 0)

  it "setEnv / lookupEnv / unsetEnv round-trips a variable" do
    out <- runProc do
      setEnv varName "abc-123"
      after <- lookupEnv varName
      env <- getEnv
      unsetEnv varName
      cleared <- lookupEnv varName
      pure { after, present: Object.member varName env, cleared }
    out.after `shouldSatisfy` isJust
    out.present `shouldEqual` true
    out.cleared `shouldEqual` Nothing
