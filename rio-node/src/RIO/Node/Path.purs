-- | Pure path-manipulation wrappers around `Node.Path`.
-- |
-- | The Node `path` module is almost entirely pure (string in,
-- | string out); only `path.resolve` reads the process's current
-- | working directory. The pure functions are re-exported verbatim,
-- | and `resolve` is exposed as an `RIO` computation lifted from
-- | `Effect`.
-- |
-- | The point of this module is convenience and discoverability
-- | (`import RIO.Node.Path as Path`) rather than any extra typing
-- | or service-row machinery. Callers who want the raw bindings
-- | can still reach `Node.Path` directly.
module RIO.Node.Path
  ( module Exports
  , PathParts
  , basename
  , basenameWithoutExt
  , concat
  , delimiter
  , dirname
  , extname
  , isAbsolute
  , normalize
  , parse
  , relative
  , resolve
  , sep
  ) where

import Effect.Class (liftEffect)
import Node.Path (FilePath) as Exports
import Node.Path (FilePath)
import Node.Path as NP

import RIO.Core (RIO)

-- | The record shape returned by `parse`. Identical to the record
-- | type from `Node.Path.parse`, named here so callers can write
-- | signatures without inlining the row.
type PathParts =
  { root :: String
  , dir :: String
  , base :: String
  , ext :: String
  , name :: String
  }

-- | Normalize a path: collapse `..` / `.`, deduplicate separators,
-- | preserve trailing slashes. Uses backslashes on Windows.
normalize :: FilePath -> FilePath
normalize = NP.normalize

-- | Join path segments and normalize the result.
concat :: Array FilePath -> FilePath
concat = NP.concat

-- | Resolve a target path against the supplied prefixes and the
-- | process's current working directory. Lifted into `RIO` because
-- | the underlying `Node.Path.resolve` is `Effect`-valued.
resolve
  :: forall r e
   . Array FilePath
  -> FilePath
  -> RIO r e FilePath
resolve prefixes target = liftEffect (NP.resolve prefixes target)

-- | Compute the relative path from `from` to `to`.
relative :: FilePath -> FilePath -> FilePath
relative = NP.relative

-- | Return the directory portion of a path.
dirname :: FilePath -> FilePath
dirname = NP.dirname

-- | Return the last component of a path (file name with extension).
basename :: FilePath -> FilePath
basename = NP.basename

-- | Return the last component of a path with the given extension
-- | stripped, if it matches.
basenameWithoutExt :: FilePath -> FilePath -> FilePath
basenameWithoutExt = NP.basenameWithoutExt

-- | Return the file extension of a path (including the leading
-- | dot, or empty if none).
extname :: FilePath -> FilePath
extname = NP.extname

-- | The platform-specific file separator (`/` on POSIX, `\\` on
-- | Windows).
sep :: String
sep = NP.sep

-- | The platform-specific PATH-list delimiter (`:` on POSIX, `;`
-- | on Windows).
delimiter :: String
delimiter = NP.delimiter

-- | Parse a path into its components.
parse :: String -> PathParts
parse = NP.parse

-- | Whether a path is absolute.
isAbsolute :: String -> Boolean
isAbsolute = NP.isAbsolute
