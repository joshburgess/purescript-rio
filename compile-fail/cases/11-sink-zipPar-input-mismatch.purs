-- Case: `Sink.zipPar` joins two sinks that read the *same* input
-- element type. If the two sides read different inputs, the
-- compiler must reject the call because `i` cannot unify.
--
-- Here `sumInts` is `Sink _ _ Int Int` (sums incoming Ints) and
-- `concatStrings` is `Sink _ _ String String` (concatenates
-- incoming Strings). Their input element types are Int vs String.
-- `zipPar sumInts concatStrings` must not typecheck.
module Scratch where

import Prelude

import Data.Tuple (Tuple)

import RIO.Sink (Sink, foldL, zipPar)

sumInts :: forall r e. Sink r e Int Int
sumInts = foldL 0 (\acc i -> acc + i)

concatStrings :: forall r e. Sink r e String String
concatStrings = foldL "" (\acc s -> acc <> s)

-- The compiler must reject this: the two sinks read different
-- element types and `zipPar`'s shared `i` cannot unify Int with
-- String.
mismatch :: forall r e. Sink r e Int (Tuple Int String)
mismatch = zipPar sumInts concatStrings
