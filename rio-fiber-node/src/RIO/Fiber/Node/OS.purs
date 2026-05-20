-- | An `OS` service exposing the read-only and priority-control
-- | surface of `Node.OS` as `Effect`-valued service ops.
-- |
-- | Most of the Node `os` API is observation rather than action
-- | (arch, hostname, uptime, free memory, network interfaces).
-- | Wrapping them as a service gives tests a hook to fake the
-- | values when behaviour depends on the host environment;
-- | production code provides `liveOS`, which delegates to
-- | `Node.OS` directly.
-- |
-- | A handful of constants from `Node.OS` are pure values (`eol`,
-- | `devNull`); they are re-exported without going through the
-- | service.
module RIO.Fiber.Node.OS
  ( OS
  , arch
  , constants
  , cpus
  , devNull
  , endianness
  , eol
  , freemem
  , getCurrentProcessPriority
  , getPriority
  , homedir
  , hostname
  , liveOS
  , loadavg
  , machine
  , networkInterfaces
  , platform
  , release
  , setCurrentProcessPriority
  , setPriority
  , tmpdir
  , totalmem
  , uptime
  , userInfo
  , version
  , module Exports
  ) where

import Prelude

import Data.Posix (Pid)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.OS (Arch, CPU, Endianness, LoadAvg, NetworkInterface, UserInfo) as Exports
import Node.OS (Arch, CPU, Endianness, LoadAvg, NetworkInterface, UserInfo)
import Node.OS as NOS

import RIO.Fiber.Core (RIO, ask, liftEffect)

-- | The service record. All operations are `Effect`-valued; smart
-- | constructors lift them into `RIO`.
type OS =
  { arch :: Effect Arch
  , cpus :: Effect (Array CPU)
  , endianness :: Effect Endianness
  , freemem :: Effect Int
  , getCurrentProcessPriority :: Effect Int
  , getPriority :: Pid -> Effect Int
  , homedir :: Effect String
  , hostname :: Effect String
  , loadavg :: Effect LoadAvg
  , machine :: Effect String
  , networkInterfaces :: Effect (Object (Array NetworkInterface))
  , platform :: Effect String
  , release :: Effect String
  , setCurrentProcessPriority :: Int -> Effect Unit
  , setPriority :: Pid -> Int -> Effect Unit
  , tmpdir :: Effect String
  , totalmem :: Effect Int
  , uptime :: Effect Number
  , userInfo :: Effect (UserInfo String)
  , version :: Effect String
  }

withOS
  :: forall r e a
   . (OS -> Effect a)
  -> RIO (os :: OS | r) e a
withOS k = do
  { os } <- ask
  liftEffect (k os)

-- | The operating-system end-of-line marker. Pure (`Node.OS.eol`
-- | is a top-level `String`), so it sits outside the service.
eol :: String
eol = NOS.eol

-- | The path of the null device on the current platform
-- | (`/dev/null`, `\\.\nul`). Pure.
devNull :: String
devNull = NOS.devNull

-- | Common OS-specific constants (signal codes, errno values,
-- | priority constants, etc.). Pure read from `Node.OS.constants`.
constants :: Object Foreign
constants = NOS.constants

-- | Architecture for which Node was compiled.
arch :: forall r e. RIO (os :: OS | r) e Arch
arch = withOS _.arch

-- | Per-core CPU information.
cpus :: forall r e. RIO (os :: OS | r) e (Array CPU)
cpus = withOS _.cpus

-- | Endianness of the host CPU.
endianness :: forall r e. RIO (os :: OS | r) e Endianness
endianness = withOS _.endianness

-- | Free system memory in bytes.
freemem :: forall r e. RIO (os :: OS | r) e Int
freemem = withOS _.freemem

-- | Scheduling priority of the current process.
getCurrentProcessPriority :: forall r e. RIO (os :: OS | r) e Int
getCurrentProcessPriority = withOS _.getCurrentProcessPriority

-- | Scheduling priority of the named process.
getPriority :: forall r e. Pid -> RIO (os :: OS | r) e Int
getPriority pid = withOS \os -> os.getPriority pid

-- | Path of the current user's home directory.
homedir :: forall r e. RIO (os :: OS | r) e String
homedir = withOS _.homedir

-- | Network hostname of the OS.
hostname :: forall r e. RIO (os :: OS | r) e String
hostname = withOS _.hostname

-- | One- / five- / fifteen-minute load averages (Unix); zeros on
-- | Windows.
loadavg :: forall r e. RIO (os :: OS | r) e LoadAvg
loadavg = withOS _.loadavg

-- | Machine architecture string from `uname(3)` (or equivalent).
machine :: forall r e. RIO (os :: OS | r) e String
machine = withOS _.machine

-- | Network interfaces by name, with their assigned addresses.
networkInterfaces
  :: forall r e
   . RIO (os :: OS | r) e (Object (Array NetworkInterface))
networkInterfaces = withOS _.networkInterfaces

-- | OS name (e.g. `Darwin`, `Linux`, `Windows_NT`). Maps onto
-- | `Node.OS.type_`; renamed here because `type_` is awkward as a
-- | service-method name and `platform` is what the underlying
-- | Node API calls "platform name" in most other documentation.
platform :: forall r e. RIO (os :: OS | r) e String
platform = withOS _.platform

-- | OS release string.
release :: forall r e. RIO (os :: OS | r) e String
release = withOS _.release

-- | Set the scheduling priority of the current process.
setCurrentProcessPriority
  :: forall r e
   . Int
  -> RIO (os :: OS | r) e Unit
setCurrentProcessPriority pri = withOS \os -> os.setCurrentProcessPriority pri

-- | Set the scheduling priority of the named process.
setPriority
  :: forall r e
   . Pid
  -> Int
  -> RIO (os :: OS | r) e Unit
setPriority pid pri = withOS \os -> os.setPriority pid pri

-- | OS-default temp directory.
tmpdir :: forall r e. RIO (os :: OS | r) e String
tmpdir = withOS _.tmpdir

-- | Total system memory in bytes.
totalmem :: forall r e. RIO (os :: OS | r) e Int
totalmem = withOS _.totalmem

-- | System uptime in seconds.
uptime :: forall r e. RIO (os :: OS | r) e Number
uptime = withOS _.uptime

-- | Information about the currently effective user. String form;
-- | the buffer variant is intentionally not exposed through the
-- | service. Callers who need it can reach `Node.OS.userInfoBuffer`
-- | directly.
userInfo :: forall r e. RIO (os :: OS | r) e (UserInfo String)
userInfo = withOS _.userInfo

-- | Kernel version string.
version :: forall r e. RIO (os :: OS | r) e String
version = withOS _.version

-- | A production-ready implementation that delegates each
-- | operation to `Node.OS`.
liveOS :: OS
liveOS =
  { arch: NOS.arch
  , cpus: NOS.cpus
  , endianness: NOS.endianness
  , freemem: NOS.freemem
  , getCurrentProcessPriority: NOS.getCurrentProcessPriority
  , getPriority: NOS.getPriority
  , homedir: NOS.homedir
  , hostname: NOS.hostname
  , loadavg: NOS.loadavg
  , machine: NOS.machine
  , networkInterfaces: NOS.networkInterfaces
  , platform: NOS.type_
  , release: NOS.release
  , setCurrentProcessPriority: NOS.setCurrentProcessPriority
  , setPriority: NOS.setPriority
  , tmpdir: NOS.tmpdir
  , totalmem: NOS.totalmem
  , uptime: NOS.uptime
  , userInfo: NOS.userInfo
  , version: NOS.version
  }
