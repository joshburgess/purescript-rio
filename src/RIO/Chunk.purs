-- | A chunky, catenable, immutable sequence.
-- |
-- | `Chunk` is the data structure ZIO and Effect-TS use to back
-- | their `Stream` types. It exists because the natural shape of
-- | streaming data is "a small bundle of values at a time", which
-- | `Array` handles awkwardly: concatenation is O(n+m) for
-- | `Array`, which means folding a long stream of small arrays
-- | into a single `Array` costs O(n^2) in total. `Chunk` makes the
-- | concatenation O(1) by representing the catenated shape as a
-- | small tree, then defers the linearisation cost until the
-- | caller actually asks for it.
-- |
-- | ### Big-O summary
-- |
-- |   * `empty`, `singleton`: O(1)
-- |   * `fromArray`, `toArray`: O(n)
-- |   * `prepend`, `append`: O(1)
-- |   * `concat` (`<>`): O(1)
-- |   * `length`: O(n) on the tree, but constant after the first
-- |     walk (the size is cached per `Concat` node)
-- |   * `index`: O(depth) on average
-- |   * `head`, `tail`: O(depth) average; `tail` rebuilds the
-- |     rest of the chunk
-- |   * `map`, `foldl`, `foldr`: O(n)
-- |   * `materialize`: O(n), one-shot flatten into a single
-- |     `Singleton` node so subsequent `toArray` / `index` calls
-- |     are cheap
-- |
-- | ### Internal representation
-- |
-- | A `Chunk a` is one of:
-- |
-- |   * `Empty`: the empty chunk, length 0.
-- |   * `Singleton (Array a)`: a flat array, length
-- |     `Array.length arr`. Most `Chunk`s end up in this shape
-- |     after `materialize`.
-- |   * `Concat Int (Chunk a) (Chunk a)`: the catenation of two
-- |     sub-chunks. The first field is the cached length so we
-- |     don't have to walk the tree twice.
-- |
-- | We deliberately do not balance the tree: in practice streams
-- | accrete chunks via `<>` on the right, which keeps the tree
-- | right-leaning and amortises favourably for the workloads we
-- | care about. If a pathological build-up of depth becomes an
-- | issue, callers can call `materialize` to flatten back to a
-- | single `Singleton` node.
-- |
-- | ```purescript
-- | -- O(1) concatenation:
-- | a = Chunk.fromArray [ 1, 2, 3 ]
-- | b = Chunk.fromArray [ 4, 5 ]
-- | ab = a <> b   -- O(1), no copy
-- |
-- | -- Linearise once at the end:
-- | xs = Chunk.toArray ab   -- [1, 2, 3, 4, 5]
-- | ```
module RIO.Chunk
  ( Chunk
  , empty
  , singleton
  , fromArray
  , toArray
  , isEmpty
  , length
  , prepend
  , append
  , concat
  , index
  , head
  , tail
  , map
  , foldl
  , foldr
  , materialize
  ) where

import Prelude hiding (map, append)
import Prelude as P

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))

-- | The opaque chunk type. Construct via `empty`, `singleton`,
-- | `fromArray`, or by combining existing chunks with `<>`.
data Chunk a
  = Empty
  | Singleton (Array a)
  | Concat Int (Chunk a) (Chunk a)

-- | The empty chunk.
empty :: forall a. Chunk a
empty = Empty

-- | A chunk holding exactly one value.
singleton :: forall a. a -> Chunk a
singleton a = Singleton [ a ]

-- | Wrap an `Array` as a chunk in O(1).
fromArray :: forall a. Array a -> Chunk a
fromArray xs
  | Array.null xs = Empty
  | otherwise = Singleton xs

-- | Flatten a chunk into a single `Array`. O(n) (one pass).
-- |
-- | This is the moment to pay the linearisation cost; before it,
-- | the catenated shape stays as a tree of `Concat` nodes.
toArray :: forall a. Chunk a -> Array a
toArray = case _ of
  Empty -> []
  Singleton xs -> xs
  Concat _ l r -> toArray l <> toArray r

-- | True iff the chunk holds no values.
isEmpty :: forall a. Chunk a -> Boolean
isEmpty = case _ of
  Empty -> true
  Singleton xs -> Array.null xs
  Concat n _ _ -> n == 0

-- | The number of values in the chunk. O(n) the first time on a
-- | tree-shaped chunk; `Concat` caches the result, so repeat
-- | reads are O(1).
length :: forall a. Chunk a -> Int
length = case _ of
  Empty -> 0
  Singleton xs -> Array.length xs
  Concat n _ _ -> n

-- | Prepend a single value. O(1).
prepend :: forall a. a -> Chunk a -> Chunk a
prepend a c = concat (singleton a) c

-- | Append a single value. O(1).
append :: forall a. Chunk a -> a -> Chunk a
append c a = concat c (singleton a)

-- | Concatenate two chunks. O(1) (just builds a `Concat` node).
concat :: forall a. Chunk a -> Chunk a -> Chunk a
concat Empty r = r
concat l Empty = l
concat l r = Concat (length l + length r) l r

-- | `Semigroup` is concatenation.
instance semigroupChunk :: Semigroup (Chunk a) where
  append = concat

-- | `Monoid` identity is `empty`.
instance monoidChunk :: Monoid (Chunk a) where
  mempty = empty

-- | Show is via the underlying array, for ergonomics.
instance showChunk :: Show a => Show (Chunk a) where
  show c = "Chunk " <> show (toArray c)

-- | Equality is structural over the values: two chunks compare
-- | equal when their linearised arrays do.
instance eqChunk :: Eq a => Eq (Chunk a) where
  eq a b = toArray a == toArray b

-- | Look up the value at the given index. Returns `Nothing` for
-- | out-of-range indices.
index :: forall a. Int -> Chunk a -> Maybe a
index i c
  | i < 0 || i >= length c = Nothing
  | otherwise = go i c
      where
      go :: Int -> Chunk a -> Maybe a
      go _ Empty = Nothing
      go j (Singleton xs) = Array.index xs j
      go j (Concat _ l r) =
        let
          ll = length l
        in
          if j < ll then go j l else go (j - ll) r

-- | The first value, or `Nothing` if the chunk is empty.
head :: forall a. Chunk a -> Maybe a
head = index 0

-- | Every value except the first. The result is `empty` for an
-- | empty input.
tail :: forall a. Chunk a -> Chunk a
tail c = case length c of
  0 -> empty
  _ ->
    let
      arr = toArray c
    in
      case Array.tail arr of
        Nothing -> empty
        Just rest -> fromArray rest

-- | Map a function over every value. O(n).
map :: forall a b. (a -> b) -> Chunk a -> Chunk b
map _ Empty = Empty
map f (Singleton xs) = Singleton (P.map f xs)
map f (Concat n l r) = Concat n (map f l) (map f r)

instance functorChunk :: Functor Chunk where
  map = map

-- | Left fold over the chunk.
foldl :: forall a b. (b -> a -> b) -> b -> Chunk a -> b
foldl _ z Empty = z
foldl f z (Singleton xs) = Foldable.foldl f z xs
foldl f z (Concat _ l r) = foldl f (foldl f z l) r

-- | Right fold over the chunk.
foldr :: forall a b. (a -> b -> b) -> b -> Chunk a -> b
foldr _ z Empty = z
foldr f z (Singleton xs) = Foldable.foldr f z xs
foldr f z (Concat _ l r) = foldr f (foldr f z r) l

-- | Flatten the chunk's internal tree representation into a single
-- | `Singleton` node. Useful when a chunk has built up depth from
-- | repeated catenation and a future indexed read is expected.
-- |
-- | Cost is O(n); after `materialize`, every subsequent `toArray`,
-- | `length`, and `index` is the cheap `Singleton` path.
materialize :: forall a. Chunk a -> Chunk a
materialize c = fromArray (toArray c)
