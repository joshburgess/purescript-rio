-- | RIO-flavoured wrappers around `Node.ChildProcess` and
-- | `Node.ChildProcess.Aff`.
-- |
-- | A `ChildProcess` is a value (a handle on a launched process),
-- | not a capability, so this module mirrors the `Node.ChildProcess`
-- | surface by lifting each `Effect`/`Aff` operation into `RIO` and
-- | re-exporting the types and event handles unchanged.
-- |
-- | Callback-style operations (`exec'`, `execFile'`, `send'`) keep
-- | their Node-shaped `Effect`-valued continuations. Callers who
-- | want blocking semantics can compose `RIO.Node.EventEmitter`
-- | listeners around the `exitH` / `errorH` handles or use
-- | `waitSpawned` (re-exposed here) for the spawn step.
module RIO.Node.ChildProcess
  ( module Exports
  , ForkOptions
  , SendOptions
  , SpawnSyncResult
  , connected
  , disconnect
  , exec
  , exec'
  , execFile
  , execFile'
  , execFileSync
  , execFileSync'
  , execSync
  , execSync'
  , exitCode
  , fork
  , fork'
  , kill
  , kill'
  , killSignal
  , killed
  , pid
  , pidExists
  , ref
  , send
  , send'
  , signalCode
  , spawn
  , spawn'
  , spawnSync
  , spawnSync'
  , unref
  , waitSpawned
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Posix.Signal (Signal)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Buffer (Buffer)
import Data.Posix (Gid, Pid, Uid)
import Data.Time.Duration (Milliseconds)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.Buffer (Buffer) as NodeBuffer
import Node.ChildProcess
  ( ChildProcess
  , ExecFileOptions
  , ExecFileSyncOptions
  , ExecOptions
  , ExecResult
  , ExecSyncOptions
  , SpawnOptions
  , SpawnSyncOptions
  , closeH
  , disconnectH
  , errorH
  , exitH
  , messageH
  , spawnArgs
  , spawnFile
  , spawnH
  , stderr
  , stdin
  , stdio
  , stdout
  , toEventEmitter
  , toUnsafeChildProcess
  ) as Exports
import Node.ChildProcess
  ( ChildProcess
  , ExecFileOptions
  , ExecFileSyncOptions
  , ExecOptions
  , ExecResult
  , ExecSyncOptions
  , SpawnOptions
  , SpawnSyncOptions
  )
import Node.ChildProcess as CP
import Node.ChildProcess.Aff as CPAff
import Node.ChildProcess.Types
  ( Exit(..)
  , Handle
  , KillSignal
  , Shell
  , StdIO
  , StringOrBuffer
  , UnsafeChildProcess
  , customShell
  , defaultStdIO
  , enableShell
  , fileDescriptor
  , fileDescriptor'
  , fromKillSignal
  , fromKillSignal'
  , ignore
  , inherit
  , intSignal
  , ipc
  , overlapped
  , pipe
  , shareStream
  , stringSignal
  ) as Exports
import Node.ChildProcess.Types (Exit, Handle, KillSignal, StdIO)
import Node.Errors.SystemError (SystemError) as Exports
import Node.Errors.SystemError (SystemError)

import RIO.Core (RIO)

-- | Options for `fork'`. Mirrors `Node.ChildProcess.ForkOptions`,
-- | which isn't exported from the upstream module.
type ForkOptions =
  { cwd :: Maybe String
  , detached :: Maybe Boolean
  , appendStdio :: Maybe (Array StdIO)
  , env :: Maybe (Object String)
  , execPath :: Maybe String
  , execArgv :: Maybe (Array String)
  , gid :: Maybe Gid
  , serialization :: Maybe String
  , killSignal :: Maybe KillSignal
  , silent :: Maybe Boolean
  , uid :: Maybe Uid
  , windowsVerbatimArguments :: Maybe Boolean
  , timeout :: Maybe Milliseconds
  }

-- | Options for `send'`. Mirrors `Node.ChildProcess.SendOptions`,
-- | which isn't exported from the upstream module.
type SendOptions = { keepAlive :: Maybe Boolean }

-- | The record returned by `spawnSync`. Mirrors
-- | `Node.ChildProcess.SpawnSyncResult`, which isn't exported from
-- | the upstream module.
type SpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: NodeBuffer.Buffer
  , stderr :: NodeBuffer.Buffer
  , exitStatus :: Exit
  , error :: Maybe Exports.SystemError
  }

-- | Spawn a child process by running an executable with the given
-- | arguments. The default `stdio` is `[pipe, pipe, pipe]`.
spawn :: forall r e. String -> Array String -> RIO r e ChildProcess
spawn cmd args = liftEffect (CP.spawn cmd args)

-- | `spawn` with full `SpawnOptions` configuration.
spawn'
  :: forall r e
   . String
  -> Array String
  -> (SpawnOptions -> SpawnOptions)
  -> RIO r e ChildProcess
spawn' cmd args buildOpts = liftEffect (CP.spawn' cmd args buildOpts)

-- | Synchronous `spawn`: blocks the event loop until the child
-- | process exits and returns the captured output.
spawnSync
  :: forall r e
   . String
  -> Array String
  -> RIO r e SpawnSyncResult
spawnSync cmd args = liftEffect (CP.spawnSync cmd args)

-- | `spawnSync` with full `SpawnSyncOptions` configuration.
spawnSync'
  :: forall r e
   . String
  -> Array String
  -> (SpawnSyncOptions -> SpawnSyncOptions)
  -> RIO r e SpawnSyncResult
spawnSync' cmd args buildOpts = liftEffect (CP.spawnSync' cmd args buildOpts)

-- | Run a command through the shell, buffering output until exit.
-- | The returned `ChildProcess` will fire `exitH` once finished.
exec :: forall r e. String -> RIO r e ChildProcess
exec cmd = liftEffect (CP.exec cmd)

-- | `exec` with options and an `ExecResult` continuation.
exec'
  :: forall r e
   . String
  -> (ExecOptions -> ExecOptions)
  -> (ExecResult -> Effect Unit)
  -> RIO r e ChildProcess
exec' cmd buildOpts cb = liftEffect (CP.exec' cmd buildOpts cb)

-- | Synchronous `exec`: blocks the event loop until the command
-- | finishes. Returns the captured stdout as a `Buffer`.
execSync :: forall r e. String -> RIO r e Buffer
execSync cmd = liftEffect (CP.execSync cmd)

-- | `execSync` with full `ExecSyncOptions` configuration.
execSync'
  :: forall r e
   . String
  -> (ExecSyncOptions -> ExecSyncOptions)
  -> RIO r e Buffer
execSync' cmd buildOpts = liftEffect (CP.execSync' cmd buildOpts)

-- | Run an executable directly (no shell), buffering output.
execFile
  :: forall r e
   . String
  -> Array String
  -> RIO r e ChildProcess
execFile cmd args = liftEffect (CP.execFile cmd args)

-- | `execFile` with options and an `ExecResult` continuation.
execFile'
  :: forall r e
   . String
  -> Array String
  -> (ExecFileOptions -> ExecFileOptions)
  -> (ExecResult -> Effect Unit)
  -> RIO r e ChildProcess
execFile' cmd args buildOpts cb =
  liftEffect (CP.execFile' cmd args buildOpts cb)

-- | Synchronous `execFile`: blocks the event loop until the command
-- | finishes. Returns the captured stdout as a `Buffer`.
execFileSync
  :: forall r e
   . String
  -> Array String
  -> RIO r e Buffer
execFileSync cmd args = liftEffect (CP.execFileSync cmd args)

-- | `execFileSync` with full `ExecFileSyncOptions` configuration.
execFileSync'
  :: forall r e
   . String
  -> Array String
  -> (ExecFileSyncOptions -> ExecFileSyncOptions)
  -> RIO r e Buffer
execFileSync' cmd args buildOpts =
  liftEffect (CP.execFileSync' cmd args buildOpts)

-- | Fork a child Node.js process running the given module.
fork :: forall r e. String -> Array String -> RIO r e ChildProcess
fork modulePath args = liftEffect (CP.fork modulePath args)

-- | `fork` with full options.
fork'
  :: forall r e
   . String
  -> Array String
  -> (ForkOptions -> ForkOptions)
  -> RIO r e ChildProcess
fork' modulePath args buildOpts =
  liftEffect (CP.fork' modulePath args buildOpts)

-- | Send an IPC message to a `fork`ed child.
send
  :: forall props r e
   . { | props }
  -> Maybe Handle
  -> ChildProcess
  -> RIO r e Boolean
send msg handle cp = liftEffect (CP.send msg handle cp)

-- | `send` with options and a delivery continuation.
send'
  :: forall props r e
   . { | props }
  -> Maybe Handle
  -> (SendOptions -> SendOptions)
  -> (Maybe Error -> Effect Unit)
  -> ChildProcess
  -> RIO r e Boolean
send' msg handle buildOpts cb cp =
  liftEffect (CP.send' msg handle buildOpts cb cp)

-- | The process id, available once the child has spawned.
pid :: forall r e. ChildProcess -> RIO r e (Maybe Pid)
pid cp = liftEffect (CP.pid cp)

-- | Whether the OS still reports the child's pid as live.
pidExists :: forall r e. ChildProcess -> RIO r e Boolean
pidExists cp = liftEffect (CP.pidExists cp)

-- | Whether the IPC channel to the child is still open.
connected :: forall r e. ChildProcess -> RIO r e Boolean
connected cp = liftEffect (CP.connected cp)

-- | The numeric exit code, once the child has exited normally.
exitCode :: forall r e. ChildProcess -> RIO r e (Maybe Int)
exitCode cp = liftEffect (CP.exitCode cp)

-- | Close the IPC channel.
disconnect :: forall r e. ChildProcess -> RIO r e Unit
disconnect cp = liftEffect (CP.disconnect cp)

-- | Send `SIGTERM` to the child (or its OS default).
kill :: forall r e. ChildProcess -> RIO r e Boolean
kill cp = liftEffect (CP.kill cp)

-- | Send a specific `KillSignal` to the child.
kill' :: forall r e. KillSignal -> ChildProcess -> RIO r e Boolean
kill' sig cp = liftEffect (CP.kill' sig cp)

-- | Send a named POSIX signal to the child.
killSignal :: forall r e. Signal -> ChildProcess -> RIO r e Boolean
killSignal sig cp = liftEffect (CP.killSignal sig cp)

-- | Whether `kill` has been called on this child.
killed :: forall r e. ChildProcess -> RIO r e Boolean
killed cp = liftEffect (CP.killed cp)

-- | Keep the parent's event loop alive while the child runs.
ref :: forall r e. ChildProcess -> RIO r e Unit
ref cp = liftEffect (CP.ref cp)

-- | Stop holding the event loop open for this child.
unref :: forall r e. ChildProcess -> RIO r e Unit
unref cp = liftEffect (CP.unref cp)

-- | The signal name that terminated the child, if any.
signalCode :: forall r e. ChildProcess -> RIO r e (Maybe String)
signalCode cp = liftEffect (CP.signalCode cp)

-- | Block until the child either spawns (returning its `Pid`) or
-- | fails to start (returning the system error).
waitSpawned
  :: forall r e
   . ChildProcess
  -> RIO r e (Either SystemError Pid)
waitSpawned cp = liftAff (CPAff.waitSpawned cp)
