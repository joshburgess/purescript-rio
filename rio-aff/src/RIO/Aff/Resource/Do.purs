-- | Qualified-do sugar over `RIO.Aff.Resource.acquireRelease`.
-- |
-- | Each `<-` inside a `Resource.do` block desugars to an
-- | `acquireRelease`, with the continuation of the block becoming
-- | the `use` callback. This flattens what would otherwise be a
-- | ladder of nested brackets when a single computation needs to
-- | open several resources before using them.
-- |
-- | ```purescript
-- | import RIO.Aff.Resource.Do as Resource
-- |
-- | example :: forall r e. RIO r e Report
-- | example = Resource.do
-- |   h    <- Resource.acquire openHandle closeHandle
-- |   conn <- Resource.acquire openConn   closeConn
-- |   pool <- Resource.acquire openPool   closePool
-- |   buildReport h conn pool
-- | ```
-- |
-- | desugars to exactly:
-- |
-- | ```purescript
-- | acquireRelease openHandle closeHandle \h ->
-- |   acquireRelease openConn closeConn \conn ->
-- |     acquireRelease openPool closePool \pool ->
-- |       buildReport h conn pool
-- | ```
-- |
-- | The release ordering matches `acquireRelease`: LIFO, with
-- | every release running on every termination path (success,
-- | typed failure, defect, kill).
-- |
-- | The block as a whole has type `RIO r e b` (the type of its
-- | trailing expression). Only the right-hand side of each `<-`
-- | is an `Acquire`; the trailing expression that closes the
-- | block is a plain `RIO` action that may reference the bound
-- | resources.
-- |
-- | Plain `RIO` statements interleaved between acquisitions need
-- | an explicit `liftRIO` wrap because every `<-` must produce
-- | an `Acquire`:
-- |
-- | ```purescript
-- | Resource.do
-- |   h <- Resource.acquire openHandle closeHandle
-- |   _ <- Resource.liftRIO (logInfo "opened handle")
-- |   useHandle h
-- | ```
module RIO.Aff.Resource.Do
  ( Acquire
  , acquire
  , bind
  , discard
  , liftRIO
  , pure
  ) where

import Prelude (Unit, unit)
import Prelude (pure) as P

import RIO.Aff.Core (RIO)
import RIO.Aff.Resource (acquireRelease)

-- | The right-hand-side shape of a `<-` inside a `Resource.do`
-- | block: an acquire action paired with its release.
-- |
-- | Built with `acquire` for the common case, or `liftRIO` when
-- | a plain `RIO` action needs to appear between two
-- | acquisitions.
-- |
-- | The data constructor is intentionally hidden: outside this
-- | module, you should always build `Acquire` values through
-- | `acquire` or `liftRIO`, so the release callback's type stays
-- | `a -> RIO r () Unit` (consistent with
-- | `RIO.Aff.Resource.acquireRelease`).
newtype Acquire r e a = Acquire
  { acquire :: RIO r e a
  , release :: a -> RIO r () Unit
  }

-- | Build an `Acquire` from an acquire action and its release.
-- |
-- | The release receives the value produced by `acquire` and
-- | runs in the underlying `Aff` bracket's release phase, which
-- | is uninterruptible: a kill landing during release is queued
-- | until it completes. Release's error row is `()` because
-- | there is no caller-visible place to surface a typed failure
-- | from cleanup.
acquire
  :: forall r e a
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> Acquire r e a
acquire acq rel = Acquire { acquire: acq, release: rel }

-- | Lift a plain `RIO` action into an `Acquire` with a no-op
-- | release. Useful for interleaving non-resource statements
-- | inside a `Resource.do` block without breaking the bind shape.
liftRIO :: forall r e a. RIO r e a -> Acquire r e a
liftRIO m = Acquire { acquire: m, release: \_ -> P.pure unit }

-- | The qualified-do desugaring target for `<-`.
-- |
-- | `Resource.bind (Resource.acquire acq rel) k` is exactly
-- | `acquireRelease acq rel k`. The block's overall type is
-- | `RIO r e b` because `bind` returns `RIO r e b`; only the
-- | RHS of each `<-` is an `Acquire`. This is what lets the
-- | final line at the bottom of the block be a plain `RIO`
-- | action that references all of the bound resources.
bind
  :: forall r e a b
   . Acquire r e a
  -> (a -> RIO r e b)
  -> RIO r e b
bind (Acquire { acquire: acq, release }) k =
  acquireRelease acq release k

-- | The qualified-do desugaring target for `_ <-`. Releases the
-- | acquired resource at block end without binding it to a name.
discard
  :: forall r e a b
   . Acquire r e a
  -> (Unit -> RIO r e b)
  -> RIO r e b
discard res k = bind res (\_ -> k unit)

-- | Re-export of `Prelude.pure` so a `Resource.do` block whose
-- | trailing expression is `Resource.pure x` resolves through
-- | this module.
pure :: forall r e a. a -> RIO r e a
pure = P.pure
