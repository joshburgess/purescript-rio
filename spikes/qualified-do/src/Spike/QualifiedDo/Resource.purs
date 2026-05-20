-- | Qualified-do candidate: `Resource.do` flattens nested
-- | `acquireRelease` calls into a single flat block.
-- |
-- | The cost of `acquireRelease` today is indentation: each new
-- | resource adds a layer of nesting. With this module imported
-- | qualified, you can write:
-- |
-- | ```purescript
-- | import Spike.QualifiedDo.Resource as Resource
-- |
-- | example = Resource.do
-- |   h    <- Resource.acquire openHandle closeHandle
-- |   conn <- Resource.acquire openConn   closeConn
-- |   body h conn
-- | ```
-- |
-- | which desugars to the equivalent of:
-- |
-- | ```purescript
-- | acquireRelease openHandle closeHandle \h ->
-- |   acquireRelease openConn closeConn \conn ->
-- |     body h conn
-- | ```
-- |
-- | The release ordering matches `acquireRelease`: LIFO with
-- | release running on every termination path (success, typed
-- | failure, defect, kill).
-- |
-- | The block as a whole has type `RIO r e b` (the body's type);
-- | only the right-hand side of each `<-` is an `Acquire`.
module Spike.QualifiedDo.Resource
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
-- | Built with `acquire`. The block's `bind` calls
-- | `RIO.Aff.Resource.acquireRelease` on each one, so release is
-- | scheduled in the underlying `Aff` bracket the moment the
-- | continuation begins.
newtype Acquire r e a = Acquire
  { acquire :: RIO r e a
  , release :: a -> RIO r () Unit
  }

-- | Construct an `Acquire` from an acquire action and its
-- | release.
acquire
  :: forall r e a
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> Acquire r e a
acquire acq rel = Acquire { acquire: acq, release: rel }

-- | `Resource.bind`: the qualified-do desugaring target for `<-`.
-- |
-- | The whole `Resource.do` block has type `RIO r e b` because
-- | `bind` returns `RIO r e b`; only the RHS of each `<-` is an
-- | `Acquire`. This is what lets the body line at the bottom of
-- | the block be a plain `RIO` action.
bind
  :: forall r e a b
   . Acquire r e a
  -> (a -> RIO r e b)
  -> RIO r e b
bind (Acquire { acquire: acq, release }) k =
  acquireRelease acq release k

-- | `Resource.discard`: lets you write `_ <- acquire ...` and have
-- | the resource released at block end without binding it.
discard
  :: forall r e a b
   . Acquire r e a
  -> (Unit -> RIO r e b)
  -> RIO r e b
discard res k =
  bind res (\_ -> k unit)

-- | Re-export of `Prelude.pure` so `Resource.do` blocks that end
-- | with `Resource.pure x` resolve through this module.
pure :: forall r e a. a -> RIO r e a
pure = P.pure

-- | Lift a plain `RIO` action into an `Acquire` with a no-op
-- | release. Useful for interleaving non-resource statements
-- | inside a `Resource.do` block:
-- |
-- | ```purescript
-- | Resource.do
-- |   h <- Resource.acquire openFile closeFile
-- |   _ <- Resource.liftRIO (logInfo "opened")
-- |   body h
-- | ```
liftRIO :: forall r e a. RIO r e a -> Acquire r e a
liftRIO m = Acquire { acquire: m, release: \_ -> P.pure unit }
