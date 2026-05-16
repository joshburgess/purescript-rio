-- | A `FileSystem` service plus a live implementation backed by
-- | `node-fs`'s `Aff` interface.
-- |
-- | The shape follows the convention in `docs/02-services.md`: the
-- | service record holds `Aff`-valued operations, and the smart
-- | constructors lift them into `RIO`. The full surface of
-- | `Node.FS.Aff` is exposed, including the primed variants for
-- | options (`mkdir'`, `rm'`, `rmdir'`, `mkdtemp'`, `realpath'`,
-- | `access'`, `copyFile'`) and the file-descriptor primitives
-- | (`fdOpen`, `fdRead`, `fdNext`, `fdWrite`, `fdAppend`,
-- | `fdClose`).
-- |
-- | Errors from Node come through the underlying `Aff`'s exception
-- | channel, just like the raw `Node.FS.Aff` functions; the typed
-- | error row is left polymorphic. Callers who want typed shapes
-- | (`ENOENT` etc.) can wrap operations with `try` and use
-- | `RIO.Error`.
module RIO.Node.FileSystem
  ( FileSystem
  , access
  , access'
  , appendFile
  , appendTextFile
  , chmod
  , chown
  , copyFile
  , copyFile'
  , fdAppend
  , fdClose
  , fdNext
  , fdOpen
  , fdRead
  , fdWrite
  , lstat
  , link
  , mkdir
  , mkdir'
  , mkdtemp
  , mkdtemp'
  , readFile
  , readTextFile
  , readdir
  , readlink
  , realpath
  , realpath'
  , rename
  , rm
  , rm'
  , rmdir
  , rmdir'
  , stat
  , symlink
  , truncate
  , unlink
  , utimes
  , writeFile
  , writeTextFile
  , liveFileSystem
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (liftAff)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS (BufferLength, BufferOffset, ByteCount, FileDescriptor, FileFlags, FileMode, FilePosition, SymlinkType) as FS
import Node.FS.Aff as FSAff
import Node.FS.Constants (AccessMode, CopyMode) as FS
import Node.FS.Perms (Perms) as FS
import Node.FS.Stats (Stats) as FS
import Node.Path (FilePath)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

-- | The service record. One field per operation in `Node.FS.Aff`.
-- |
-- | Each operation returns `Aff`, matching the source library; the
-- | smart constructors below lift them into `RIO` against a
-- | `(fs :: FileSystem | r)` row.
type FileSystem =
  { access :: FilePath -> Aff (Maybe Error)
  , access' :: FilePath -> FS.AccessMode -> Aff (Maybe Error)
  , appendFile :: FilePath -> Buffer -> Aff Unit
  , appendTextFile :: Encoding -> FilePath -> String -> Aff Unit
  , chmod :: FilePath -> FS.Perms -> Aff Unit
  , chown :: FilePath -> Int -> Int -> Aff Unit
  , copyFile :: FilePath -> FilePath -> Aff Unit
  , copyFile' :: FilePath -> FilePath -> FS.CopyMode -> Aff Unit
  , fdAppend :: FS.FileDescriptor -> Buffer -> Aff FS.ByteCount
  , fdClose :: FS.FileDescriptor -> Aff Unit
  , fdNext :: FS.FileDescriptor -> Buffer -> Aff FS.ByteCount
  , fdOpen :: FilePath -> FS.FileFlags -> Maybe FS.FileMode -> Aff FS.FileDescriptor
  , fdRead ::
      FS.FileDescriptor
      -> Buffer
      -> FS.BufferOffset
      -> FS.BufferLength
      -> Maybe FS.FilePosition
      -> Aff FS.ByteCount
  , fdWrite ::
      FS.FileDescriptor
      -> Buffer
      -> FS.BufferOffset
      -> FS.BufferLength
      -> Maybe FS.FilePosition
      -> Aff FS.ByteCount
  , lstat :: FilePath -> Aff FS.Stats
  , link :: FilePath -> FilePath -> Aff Unit
  , mkdir :: FilePath -> Aff Unit
  , mkdir' :: FilePath -> { recursive :: Boolean, mode :: FS.Perms } -> Aff Unit
  , mkdtemp :: String -> Aff String
  , mkdtemp' :: String -> Encoding -> Aff String
  , readFile :: FilePath -> Aff Buffer
  , readTextFile :: Encoding -> FilePath -> Aff String
  , readdir :: FilePath -> Aff (Array FilePath)
  , readlink :: FilePath -> Aff FilePath
  , realpath :: FilePath -> Aff FilePath
  , realpath' :: FilePath -> { | () } -> Aff FilePath
  , rename :: FilePath -> FilePath -> Aff Unit
  , rm :: FilePath -> Aff Unit
  , rm' ::
      FilePath
      -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }
      -> Aff Unit
  , rmdir :: FilePath -> Aff Unit
  , rmdir' :: FilePath -> { maxRetries :: Int, retryDelay :: Int } -> Aff Unit
  , stat :: FilePath -> Aff FS.Stats
  , symlink :: FilePath -> FilePath -> FS.SymlinkType -> Aff Unit
  , truncate :: FilePath -> Int -> Aff Unit
  , unlink :: FilePath -> Aff Unit
  , utimes :: FilePath -> DateTime -> DateTime -> Aff Unit
  , writeFile :: FilePath -> Buffer -> Aff Unit
  , writeTextFile :: Encoding -> FilePath -> String -> Aff Unit
  }

_fs :: Proxy "fs"
_fs = Proxy

-- | Read service ops from the env and run them in `Aff`. Single
-- | helper so each smart constructor is one line.
withFs
  :: forall r e a
   . (FileSystem -> Aff a)
  -> RIO (fs :: FileSystem | r) e a
withFs k = do
  fs <- ask _fs
  liftAff (k fs)

-- | Test a path for accessibility with the default mode. Returns
-- | `Just err` when access fails, `Nothing` when it succeeds.
access :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e (Maybe Error)
access p = withFs \fs -> fs.access p

-- | Test a path with an explicit `AccessMode`.
access'
  :: forall r e
   . FilePath
  -> FS.AccessMode
  -> RIO (fs :: FileSystem | r) e (Maybe Error)
access' p m = withFs \fs -> fs.access' p m

-- | Append a buffer's contents to a file.
appendFile :: forall r e. FilePath -> Buffer -> RIO (fs :: FileSystem | r) e Unit
appendFile p b = withFs \fs -> fs.appendFile p b

-- | Append text to a file using the given encoding.
appendTextFile
  :: forall r e
   . Encoding
  -> FilePath
  -> String
  -> RIO (fs :: FileSystem | r) e Unit
appendTextFile enc p s = withFs \fs -> fs.appendTextFile enc p s

-- | Change the permissions of a file.
chmod :: forall r e. FilePath -> FS.Perms -> RIO (fs :: FileSystem | r) e Unit
chmod p perms = withFs \fs -> fs.chmod p perms

-- | Change the ownership of a file. `uid` and `gid` are passed as
-- | numeric ids.
chown
  :: forall r e
   . FilePath
  -> Int
  -> Int
  -> RIO (fs :: FileSystem | r) e Unit
chown p uid gid = withFs \fs -> fs.chown p uid gid

-- | Copy a file from `src` to `dest`.
copyFile
  :: forall r e
   . FilePath
  -> FilePath
  -> RIO (fs :: FileSystem | r) e Unit
copyFile src dest = withFs \fs -> fs.copyFile src dest

-- | Copy a file with an explicit `CopyMode`.
copyFile'
  :: forall r e
   . FilePath
  -> FilePath
  -> FS.CopyMode
  -> RIO (fs :: FileSystem | r) e Unit
copyFile' src dest mode = withFs \fs -> fs.copyFile' src dest mode

-- | Append a buffer to a file descriptor at the current position.
fdAppend
  :: forall r e
   . FS.FileDescriptor
  -> Buffer
  -> RIO (fs :: FileSystem | r) e FS.ByteCount
fdAppend fd buf = withFs \fs -> fs.fdAppend fd buf

-- | Close a file descriptor.
fdClose :: forall r e. FS.FileDescriptor -> RIO (fs :: FileSystem | r) e Unit
fdClose fd = withFs \fs -> fs.fdClose fd

-- | Fill the buffer from the current position of the file
-- | descriptor.
fdNext
  :: forall r e
   . FS.FileDescriptor
  -> Buffer
  -> RIO (fs :: FileSystem | r) e FS.ByteCount
fdNext fd buf = withFs \fs -> fs.fdNext fd buf

-- | Open a file. When `mode` is `Nothing`, Node's default mode is
-- | used.
fdOpen
  :: forall r e
   . FilePath
  -> FS.FileFlags
  -> Maybe FS.FileMode
  -> RIO (fs :: FileSystem | r) e FS.FileDescriptor
fdOpen p flags mode = withFs \fs -> fs.fdOpen p flags mode

-- | Read from a file descriptor at an explicit position.
fdRead
  :: forall r e
   . FS.FileDescriptor
  -> Buffer
  -> FS.BufferOffset
  -> FS.BufferLength
  -> Maybe FS.FilePosition
  -> RIO (fs :: FileSystem | r) e FS.ByteCount
fdRead fd buf off len pos = withFs \fs -> fs.fdRead fd buf off len pos

-- | Write to a file descriptor at an explicit position.
fdWrite
  :: forall r e
   . FS.FileDescriptor
  -> Buffer
  -> FS.BufferOffset
  -> FS.BufferLength
  -> Maybe FS.FilePosition
  -> RIO (fs :: FileSystem | r) e FS.ByteCount
fdWrite fd buf off len pos = withFs \fs -> fs.fdWrite fd buf off len pos

-- | Like `stat` but for symlinks: stats the link itself rather
-- | than what it points at.
lstat :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e FS.Stats
lstat p = withFs \fs -> fs.lstat p

-- | Create a hard link.
link
  :: forall r e
   . FilePath
  -> FilePath
  -> RIO (fs :: FileSystem | r) e Unit
link src dest = withFs \fs -> fs.link src dest

-- | Create a directory.
mkdir :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e Unit
mkdir p = withFs \fs -> fs.mkdir p

-- | Create a directory with `recursive` / `mode` options.
mkdir'
  :: forall r e
   . FilePath
  -> { recursive :: Boolean, mode :: FS.Perms }
  -> RIO (fs :: FileSystem | r) e Unit
mkdir' p opts = withFs \fs -> fs.mkdir' p opts

-- | Create a temporary directory whose name starts with `prefix`.
-- | Returns the full path.
mkdtemp :: forall r e. String -> RIO (fs :: FileSystem | r) e String
mkdtemp prefix = withFs \fs -> fs.mkdtemp prefix

-- | `mkdtemp` with an explicit encoding.
mkdtemp'
  :: forall r e
   . String
  -> Encoding
  -> RIO (fs :: FileSystem | r) e String
mkdtemp' prefix enc = withFs \fs -> fs.mkdtemp' prefix enc

-- | Read the entire contents of a file as a buffer.
readFile :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e Buffer
readFile p = withFs \fs -> fs.readFile p

-- | Read the entire contents of a text file in the given
-- | encoding.
readTextFile
  :: forall r e
   . Encoding
  -> FilePath
  -> RIO (fs :: FileSystem | r) e String
readTextFile enc p = withFs \fs -> fs.readTextFile enc p

-- | List the entries of a directory.
readdir
  :: forall r e
   . FilePath
  -> RIO (fs :: FileSystem | r) e (Array FilePath)
readdir p = withFs \fs -> fs.readdir p

-- | Read the target of a symlink.
readlink :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e FilePath
readlink p = withFs \fs -> fs.readlink p

-- | Resolve a path to its canonical absolute form.
realpath :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e FilePath
realpath p = withFs \fs -> fs.realpath p

-- | `realpath` using the supplied cache object for already-resolved
-- | paths.
realpath'
  :: forall r e
   . FilePath
  -> { | () }
  -> RIO (fs :: FileSystem | r) e FilePath
realpath' p cache = withFs \fs -> fs.realpath' p cache

-- | Rename a file.
rename
  :: forall r e
   . FilePath
  -> FilePath
  -> RIO (fs :: FileSystem | r) e Unit
rename src dest = withFs \fs -> fs.rename src dest

-- | Remove a file or directory.
rm :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e Unit
rm p = withFs \fs -> fs.rm p

-- | Remove a file or directory with explicit options.
rm'
  :: forall r e
   . FilePath
  -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }
  -> RIO (fs :: FileSystem | r) e Unit
rm' p opts = withFs \fs -> fs.rm' p opts

-- | Remove a directory.
rmdir :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e Unit
rmdir p = withFs \fs -> fs.rmdir p

-- | Remove a directory with explicit retry options.
rmdir'
  :: forall r e
   . FilePath
  -> { maxRetries :: Int, retryDelay :: Int }
  -> RIO (fs :: FileSystem | r) e Unit
rmdir' p opts = withFs \fs -> fs.rmdir' p opts

-- | Get file statistics.
stat :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e FS.Stats
stat p = withFs \fs -> fs.stat p

-- | Create a symlink.
symlink
  :: forall r e
   . FilePath
  -> FilePath
  -> FS.SymlinkType
  -> RIO (fs :: FileSystem | r) e Unit
symlink src dest ty = withFs \fs -> fs.symlink src dest ty

-- | Truncate a file to the specified length.
truncate
  :: forall r e
   . FilePath
  -> Int
  -> RIO (fs :: FileSystem | r) e Unit
truncate p len = withFs \fs -> fs.truncate p len

-- | Delete a file.
unlink :: forall r e. FilePath -> RIO (fs :: FileSystem | r) e Unit
unlink p = withFs \fs -> fs.unlink p

-- | Set the accessed and modified times of a path.
utimes
  :: forall r e
   . FilePath
  -> DateTime
  -> DateTime
  -> RIO (fs :: FileSystem | r) e Unit
utimes p atime mtime = withFs \fs -> fs.utimes p atime mtime

-- | Write a buffer to a file, replacing existing contents.
writeFile
  :: forall r e
   . FilePath
  -> Buffer
  -> RIO (fs :: FileSystem | r) e Unit
writeFile p b = withFs \fs -> fs.writeFile p b

-- | Write text to a file in the given encoding, replacing
-- | existing contents.
writeTextFile
  :: forall r e
   . Encoding
  -> FilePath
  -> String
  -> RIO (fs :: FileSystem | r) e Unit
writeTextFile enc p s = withFs \fs -> fs.writeTextFile enc p s

-- | A production-ready implementation backed by `Node.FS.Aff`.
-- | Provide it via `provide` / `provideAll` or wrap it in a
-- | `Layer`.
-- |
-- | ```purescript
-- | main = launchAff_ (runRIO (provideAll { fs: liveFileSystem } program))
-- | ```
liveFileSystem :: FileSystem
liveFileSystem =
  { access: FSAff.access
  , access': FSAff.access'
  , appendFile: FSAff.appendFile
  , appendTextFile: FSAff.appendTextFile
  , chmod: FSAff.chmod
  , chown: FSAff.chown
  , copyFile: FSAff.copyFile
  , copyFile': FSAff.copyFile'
  , fdAppend: FSAff.fdAppend
  , fdClose: FSAff.fdClose
  , fdNext: FSAff.fdNext
  , fdOpen: FSAff.fdOpen
  , fdRead: FSAff.fdRead
  , fdWrite: FSAff.fdWrite
  , lstat: FSAff.lstat
  , link: FSAff.link
  , mkdir: FSAff.mkdir
  , mkdir': FSAff.mkdir'
  , mkdtemp: FSAff.mkdtemp
  , mkdtemp': FSAff.mkdtemp'
  , readFile: FSAff.readFile
  , readTextFile: FSAff.readTextFile
  , readdir: FSAff.readdir
  , readlink: FSAff.readlink
  , realpath: FSAff.realpath
  , realpath': FSAff.realpath'
  , rename: FSAff.rename
  , rm: FSAff.rm
  , rm': FSAff.rm'
  , rmdir: FSAff.rmdir
  , rmdir': FSAff.rmdir'
  , stat: FSAff.stat
  , symlink: FSAff.symlink
  , truncate: FSAff.truncate
  , unlink: FSAff.unlink
  , utimes: FSAff.utimes
  , writeFile: FSAff.writeFile
  , writeTextFile: FSAff.writeTextFile
  }
