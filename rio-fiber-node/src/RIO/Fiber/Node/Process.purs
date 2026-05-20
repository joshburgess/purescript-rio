-- | A `Process` service exposing the practical subset of
-- | `Node.Process` (argv / env / cwd / exit / pid / uptime /
-- | resource usage / process title / signal kill / nextTick) as
-- | `Effect`-valued service ops.
-- |
-- | Stream handles (`stdin`, `stdout`, `stderr`) are intentionally
-- | left out of this service; they belong with the streaming
-- | bridge (`RIO.Fiber.Node.Stream`). Event-handler bindings (`exitH`,
-- | `beforeExitH`, etc.) and the IPC `send` primitives are also
-- | out of scope here; reach `Node.Process` directly when you
-- | need them.
-- |
-- | Pure properties (`pid`, `ppid`, `platform`, `platformStr`,
-- | `version`, the TTY-detection booleans, `debugPort`) are
-- | re-exported as ordinary values, not pushed through the
-- | service row.
module RIO.Fiber.Node.Process
  ( Process
  , abort
  , argv
  , argv0
  , chdir
  , cpuUsage
  , cpuUsageDiff
  , cwd
  , execArgv
  , execPath
  , exit
  , exit'
  , getEnv
  , getExitCode
  , getGid
  , getTitle
  , getUid
  , killInt
  , killPid
  , killStr
  , liveProcess
  , lookupEnv
  , memoryUsage
  , memoryUsageRss
  , nextTick
  , resourceUsage
  , setEnv
  , setExitCode
  , setTitle
  , unsetEnv
  , uptime
  , module Exports
  , debugPort
  , pid
  , platform
  , platformStr
  , ppid
  , stderrIsTTY
  , stdinIsTTY
  , stdoutIsTTY
  , version
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Posix (Gid, Pid, Uid)
import Effect (Effect)
import Foreign.Object (Object)
import Node.Platform (Platform)
import Node.Process (CpuUsage, MemoryUsage, ResourceUsage) as Exports
import Node.Process (CpuUsage, MemoryUsage, ResourceUsage)
import Node.Process as NP

import RIO.Fiber.Core (RIO, ask, liftEffect)

-- | The service record. `abort` is a `Maybe` because the
-- | underlying Node API may not expose it in worker threads;
-- | callers receive the maybe and decide how to surface it.
type Process =
  { abort :: Maybe (Effect Unit)
  , argv :: Effect (Array String)
  , argv0 :: Effect String
  , chdir :: String -> Effect Unit
  , cpuUsage :: Effect CpuUsage
  , cpuUsageDiff :: CpuUsage -> Effect CpuUsage
  , cwd :: Effect String
  , execArgv :: Effect (Array String)
  , execPath :: Effect String
  , exit :: Effect Void
  , exit' :: Int -> Effect Void
  , getEnv :: Effect (Object String)
  , getExitCode :: Effect (Maybe Int)
  , getGid :: Effect (Maybe Gid)
  , getTitle :: Effect String
  , getUid :: Effect (Maybe Uid)
  , killInt :: Pid -> Int -> Effect Unit
  , killPid :: Pid -> Effect Unit
  , killStr :: Pid -> String -> Effect Unit
  , lookupEnv :: String -> Effect (Maybe String)
  , memoryUsage :: Effect MemoryUsage
  , memoryUsageRss :: Effect Int
  , nextTick :: Effect Unit -> Effect Unit
  , resourceUsage :: Effect ResourceUsage
  , setEnv :: String -> String -> Effect Unit
  , setExitCode :: Int -> Effect Unit
  , setTitle :: String -> Effect Unit
  , unsetEnv :: String -> Effect Unit
  , uptime :: Effect Number
  }

withProc
  :: forall r e a
   . (Process -> Effect a)
  -> RIO (process :: Process | r) e a
withProc k = do
  { process: proc } <- ask
  liftEffect (k proc)

-- | The Node.js process abort hook. `Nothing` indicates the
-- | runtime doesn't expose `process.abort` (e.g. in worker
-- | threads).
abort :: forall r e. RIO (process :: Process | r) e (Maybe (Effect Unit))
abort = do
  { process: proc } <- ask
  pure proc.abort

-- | Command-line arguments passed to Node.
argv :: forall r e. RIO (process :: Process | r) e (Array String)
argv = withProc _.argv

-- | First element of `argv`, separately addressable.
argv0 :: forall r e. RIO (process :: Process | r) e String
argv0 = withProc _.argv0

-- | Change the current working directory of the process.
chdir :: forall r e. String -> RIO (process :: Process | r) e Unit
chdir d = withProc \p -> p.chdir d

-- | A `CpuUsage` snapshot for the process.
cpuUsage :: forall r e. RIO (process :: Process | r) e CpuUsage
cpuUsage = withProc _.cpuUsage

-- | A `CpuUsage` delta relative to a previous snapshot.
cpuUsageDiff
  :: forall r e
   . CpuUsage
  -> RIO (process :: Process | r) e CpuUsage
cpuUsageDiff prev = withProc \p -> p.cpuUsageDiff prev

-- | The current working directory of the process.
cwd :: forall r e. RIO (process :: Process | r) e String
cwd = withProc _.cwd

-- | Node-specific options passed to the `node` executable.
execArgv :: forall r e. RIO (process :: Process | r) e (Array String)
execArgv = withProc _.execArgv

-- | Absolute path of the `node` binary that started the process.
execPath :: forall r e. RIO (process :: Process | r) e String
execPath = withProc _.execPath

-- | Exit the process with the previously-set exit code (or 0).
-- | Polymorphic in the result type because the program never
-- | returns from this call.
exit :: forall r e a. RIO (process :: Process | r) e a
exit = do
  { process: proc } <- ask
  v <- liftEffect proc.exit
  pure (absurd v)

-- | Exit with an explicit code. Polymorphic in the result type
-- | for the same reason as `exit`.
exit'
  :: forall r e a
   . Int
  -> RIO (process :: Process | r) e a
exit' code = do
  { process: proc } <- ask
  v <- liftEffect (proc.exit' code)
  pure (absurd v)

-- | Copy of the environment object.
getEnv
  :: forall r e
   . RIO (process :: Process | r) e (Object String)
getEnv = withProc _.getEnv

-- | The currently-set exit code, if one was previously stored.
getExitCode
  :: forall r e
   . RIO (process :: Process | r) e (Maybe Int)
getExitCode = withProc _.getExitCode

-- | Effective group id, when supported.
getGid :: forall r e. RIO (process :: Process | r) e (Maybe Gid)
getGid = withProc _.getGid

-- | Current `process.title`.
getTitle :: forall r e. RIO (process :: Process | r) e String
getTitle = withProc _.getTitle

-- | Effective user id, when supported.
getUid :: forall r e. RIO (process :: Process | r) e (Maybe Uid)
getUid = withProc _.getUid

-- | Send a signal to a process by integer signal number.
killInt
  :: forall r e
   . Pid
  -> Int
  -> RIO (process :: Process | r) e Unit
killInt p sig = withProc \pr -> pr.killInt p sig

-- | Send the default signal (SIGTERM) to a process.
killPid
  :: forall r e
   . Pid
  -> RIO (process :: Process | r) e Unit
killPid p = withProc \pr -> pr.killPid p

-- | Send a signal to a process by name (e.g. `"SIGTERM"`).
killStr
  :: forall r e
   . Pid
  -> String
  -> RIO (process :: Process | r) e Unit
killStr p sig = withProc \pr -> pr.killStr p sig

-- | Look up a single environment variable.
lookupEnv
  :: forall r e
   . String
  -> RIO (process :: Process | r) e (Maybe String)
lookupEnv k = withProc \p -> p.lookupEnv k

-- | Detailed memory-usage snapshot.
memoryUsage
  :: forall r e
   . RIO (process :: Process | r) e MemoryUsage
memoryUsage = withProc _.memoryUsage

-- | Resident-set-size only (faster than the full `memoryUsage`).
memoryUsageRss
  :: forall r e
   . RIO (process :: Process | r) e Int
memoryUsageRss = withProc _.memoryUsageRss

-- | Schedule a callback to run on the next tick of the event loop.
nextTick
  :: forall r e
   . Effect Unit
  -> RIO (process :: Process | r) e Unit
nextTick cb = withProc \p -> p.nextTick cb

-- | Resource-usage snapshot.
resourceUsage
  :: forall r e
   . RIO (process :: Process | r) e ResourceUsage
resourceUsage = withProc _.resourceUsage

-- | Set an environment variable.
setEnv
  :: forall r e
   . String
  -> String
  -> RIO (process :: Process | r) e Unit
setEnv k v = withProc \p -> p.setEnv k v

-- | Set the process's exit code without exiting immediately.
setExitCode
  :: forall r e
   . Int
  -> RIO (process :: Process | r) e Unit
setExitCode c = withProc \p -> p.setExitCode c

-- | Set `process.title`.
setTitle
  :: forall r e
   . String
  -> RIO (process :: Process | r) e Unit
setTitle t = withProc \p -> p.setTitle t

-- | Remove an environment variable.
unsetEnv
  :: forall r e
   . String
  -> RIO (process :: Process | r) e Unit
unsetEnv k = withProc \p -> p.unsetEnv k

-- | Seconds the process has been running, as a fractional Number.
uptime :: forall r e. RIO (process :: Process | r) e Number
uptime = withProc _.uptime

-- | Process id of the current process (pure).
pid :: Pid
pid = NP.pid

-- | Process id of the parent process (pure).
ppid :: Pid
ppid = NP.ppid

-- | Parsed `Platform`. `Nothing` when the underlying string
-- | doesn't match a known platform.
platform :: Maybe Platform
platform = NP.platform

-- | The raw platform string returned by Node.
platformStr :: String
platformStr = NP.platformStr

-- | Node.js version string (e.g. `"v20.10.0"`).
version :: String
version = NP.version

-- | Pure debug port reported by Node.
debugPort :: Int
debugPort = NP.debugPort

-- | Whether stdin is attached to a TTY.
stdinIsTTY :: Boolean
stdinIsTTY = NP.stdinIsTTY

-- | Whether stdout is attached to a TTY.
stdoutIsTTY :: Boolean
stdoutIsTTY = NP.stdoutIsTTY

-- | Whether stderr is attached to a TTY.
stderrIsTTY :: Boolean
stderrIsTTY = NP.stderrIsTTY

-- | A production-ready implementation backed by `Node.Process`.
liveProcess :: Process
liveProcess =
  { abort: NP.abort
  , argv: NP.argv
  , argv0: NP.argv0
  , chdir: NP.chdir
  , cpuUsage: NP.cpuUsage
  , cpuUsageDiff: NP.cpuUsageDiff
  , cwd: NP.cwd
  , execArgv: NP.execArgv
  , execPath: NP.execPath
  , exit: (NP.exit :: Effect Void)
  , exit': (NP.exit' :: Int -> Effect Void)
  , getEnv: NP.getEnv
  , getExitCode: NP.getExitCode
  , getGid: NP.getGid
  , getTitle: NP.getTitle
  , getUid: NP.getUid
  , killInt: NP.killInt
  , killPid: NP.kill
  , killStr: NP.killStr
  , lookupEnv: NP.lookupEnv
  , memoryUsage: NP.memoryUsage
  , memoryUsageRss: NP.memoryUsageRss
  , nextTick: NP.nextTick
  , resourceUsage: NP.resourceUsage
  , setEnv: NP.setEnv
  , setExitCode: NP.setExitCode
  , setTitle: NP.setTitle
  , unsetEnv: NP.unsetEnv
  , uptime: NP.uptime
  }
