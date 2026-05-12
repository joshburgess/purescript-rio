-- Case: `runRIO'` requires `RIO () () a` but receives a program whose
-- error row still has an unhandled tag.
--
-- A user who reaches for `runRIO'` before handling every typed failure
-- is making a common mistake (the row hasn't shrunk to `()`). The
-- compiler must reject this and the error message should make it
-- clear that an error row was left over.
module Scratch where

import Prelude

import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO')

failing :: RIO () (boom :: Unit) Int
failing = fail (Proxy :: Proxy "boom") unit

-- runRIO' wants `RIO () () a`. The error row here is (boom :: Unit),
-- not (). The compiler rejects the call.
result :: Aff Int
result = runRIO' failing
