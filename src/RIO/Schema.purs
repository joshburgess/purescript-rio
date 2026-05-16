-- | A small runtime schema library: bidirectional decoders /
-- | encoders for JSON, built as ordinary PureScript values.
-- |
-- | A `Schema a` carries both a `Json -> Either DecodeError a`
-- | decoder and an `a -> Json` encoder, so the same value defines
-- | the wire format in both directions. Primitive schemas
-- | (`string`, `int`, `number`, `boolean`, `null_`) describe
-- | individual JSON kinds; combinators (`array`, `nullable`,
-- | `transform`, `refine`, `union`, `enum`) compose them.
-- |
-- | Records are described through an `Applicative` builder
-- | (`RecordSchema r a`). Each `field` declares how to pull a
-- | value out of the final record and which `Schema` validates
-- | it; `recordOf` collapses the builder into a `Schema`:
-- |
-- | ```purescript
-- | type User = { name :: String, age :: Int }
-- |
-- | userSchema :: Schema User
-- | userSchema = Schema.recordOf $
-- |   { name: _, age: _ }
-- |     <$> Schema.field "name" _.name Schema.string
-- |     <*> Schema.field "age" _.age Schema.int
-- | ```
-- |
-- | `parseJson` runs a parser-and-decoder in one step, returning
-- | `ParseError` if the input string is not valid JSON. Errors
-- | carry a path (field names, array indices) which `renderError`
-- | turns into a human-readable trail; treat the trail as
-- | advisory output rather than a stable API.
module RIO.Schema
  ( Schema
  , DecodeError(..)
  , RecordSchema
  , decode
  , encode
  , parseJson
  -- Primitive schemas
  , json
  , string
  , number
  , int
  , boolean
  , null_
  , unit_
  -- Combinators
  , array
  , nullable
  , refine
  , transform
  , union
  , enum
  -- Record builder
  , field
  , fieldOpt
  , fieldDefault
  , recordOf
  -- Rendering
  , renderError
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser as Parser
import Data.Array (head, intercalate, mapWithIndex)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (foldr, find) as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | A reason a `Json` value failed to decode against a `Schema`.
-- | Paths build up as you descend through fields (`InvalidField`)
-- | and array indices (`InvalidIndex`); use `renderError` to turn
-- | a value into a human-readable trail.
data DecodeError
  = TypeMismatch { expected :: String, got :: String }
  | MissingField String
  | InvalidField String DecodeError
  | InvalidIndex Int DecodeError
  | UnionMismatch (Array DecodeError)
  | RefinementFailed String
  | ParseError String

derive instance eqDecodeError :: Eq DecodeError

instance showDecodeError :: Show DecodeError where
  show = case _ of
    TypeMismatch m ->
      "(TypeMismatch { expected: " <> show m.expected
        <> ", got: "
        <> show m.got
        <> " })"
    MissingField k -> "(MissingField " <> show k <> ")"
    InvalidField k e -> "(InvalidField " <> show k <> " " <> show e <> ")"
    InvalidIndex i e -> "(InvalidIndex " <> show i <> " " <> show e <> ")"
    UnionMismatch es -> "(UnionMismatch " <> show es <> ")"
    RefinementFailed s -> "(RefinementFailed " <> show s <> ")"
    ParseError s -> "(ParseError " <> show s <> ")"

-- | A bidirectional schema for a value of type `a`. Allocate one
-- | with the primitive constructors and the combinators in this
-- | module; consume it with `decode`, `encode`, or `parseJson`.
newtype Schema a = Schema
  { decoder :: Json -> Either DecodeError a
  , encoder :: a -> Json
  }

-- | Apply a schema's decoder.
decode :: forall a. Schema a -> Json -> Either DecodeError a
decode (Schema s) = s.decoder

-- | Apply a schema's encoder.
encode :: forall a. Schema a -> a -> Json
encode (Schema s) = s.encoder

-- | Parse a JSON string and then decode it. Returns `ParseError`
-- | if the string is not syntactically valid JSON.
parseJson :: forall a. Schema a -> String -> Either DecodeError a
parseJson s str = case Parser.jsonParser str of
  Left e -> Left (ParseError e)
  Right j -> decode s j

tag :: Json -> String
tag = Json.caseJson
  (\_ -> "null")
  (\_ -> "boolean")
  (\_ -> "number")
  (\_ -> "string")
  (\_ -> "array")
  (\_ -> "object")

-- | The identity schema. Decoding always succeeds and yields the
-- | raw `Json` value unchanged.
json :: Schema Json
json = Schema { decoder: Right, encoder: identity }

-- | A schema for JSON strings.
string :: Schema String
string = Schema
  { decoder: \j ->
      note (TypeMismatch { expected: "string", got: tag j }) (Json.toString j)
  , encoder: Json.fromString
  }

-- | A schema for JSON numbers (decoded as `Number`, i.e. an IEEE
-- | 754 double).
number :: Schema Number
number = Schema
  { decoder: \j ->
      note (TypeMismatch { expected: "number", got: tag j }) (Json.toNumber j)
  , encoder: Json.fromNumber
  }

-- | A schema for JSON numbers constrained to fit in `Int`.
-- | Non-integral or out-of-range numbers fail with
-- | `RefinementFailed`.
int :: Schema Int
int = Schema
  { decoder: \j -> case Json.toNumber j of
      Nothing -> Left (TypeMismatch { expected: "int", got: tag j })
      Just n -> case Int.fromNumber n of
        Nothing -> Left (RefinementFailed "not a finite 32-bit integer")
        Just i -> Right i
  , encoder: \i -> Json.fromNumber (Int.toNumber i)
  }

-- | A schema for JSON booleans.
boolean :: Schema Boolean
boolean = Schema
  { decoder: \j ->
      note (TypeMismatch { expected: "boolean", got: tag j }) (Json.toBoolean j)
  , encoder: Json.fromBoolean
  }

-- | A schema that accepts (and produces) JSON `null`, materialised
-- | as `Unit`.
null_ :: Schema Unit
null_ = Schema
  { decoder: \j ->
      note (TypeMismatch { expected: "null", got: tag j }) (Json.toNull j)
  , encoder: \_ -> Json.jsonNull
  }

-- | Synonym for `null_`, for readers who prefer the unit-shaped
-- | name.
unit_ :: Schema Unit
unit_ = null_

-- | Lift a per-element schema to a schema for JSON arrays. Errors
-- | inside the array are tagged with `InvalidIndex`.
array :: forall a. Schema a -> Schema (Array a)
array (Schema s) = Schema
  { decoder: \j -> case Json.toArray j of
      Nothing -> Left (TypeMismatch { expected: "array", got: tag j })
      Just arr -> traverse identity (mapWithIndex (\i x -> lmap (InvalidIndex i) (s.decoder x)) arr)
  , encoder: \xs -> Json.fromArray (map s.encoder xs)
  }

-- | Allow JSON `null` in addition to whatever `inner` accepts.
-- | Encodes `Nothing` as `null`.
nullable :: forall a. Schema a -> Schema (Maybe a)
nullable (Schema s) = Schema
  { decoder: \j ->
      if Json.isNull j then Right Nothing
      else map Just (s.decoder j)
  , encoder: maybe Json.jsonNull s.encoder
  }

-- | Attach a refinement to an existing schema. The predicate
-- | returns `Nothing` on success or `Just reason` on failure.
refine :: forall a. (a -> Maybe String) -> Schema a -> Schema a
refine check (Schema s) = Schema
  { decoder: \j -> case s.decoder j of
      Left e -> Left e
      Right a -> case check a of
        Nothing -> Right a
        Just msg -> Left (RefinementFailed msg)
  , encoder: s.encoder
  }

-- | Map a schema through an isomorphism (or a lossy pair of
-- | conversions). `from` runs after decode, `to` runs before
-- | encode.
transform :: forall a b. (a -> b) -> (b -> a) -> Schema a -> Schema b
transform from to (Schema s) = Schema
  { decoder: map from <<< s.decoder
  , encoder: s.encoder <<< to
  }

-- | Try a list of schemas in order, succeeding with the first
-- | that decodes. On total failure, returns `UnionMismatch` with
-- | every branch's error. The encoder uses the first schema's
-- | encoder; pair `union` with `transform` if you need to choose
-- | per value.
union :: forall a. Array (Schema a) -> Schema a
union schemas = Schema
  { decoder: \j ->
      let
        step (Schema s) acc = case acc of
          Right a -> Right a
          Left errs -> case s.decoder j of
            Right a -> Right a
            Left e -> Left (errs <> [ e ])
      in
        case Foldable.foldr step (Left []) schemas of
          Right a -> Right a
          Left errs -> Left (UnionMismatch errs)
  , encoder: \a -> case head schemas of
      Just (Schema s) -> s.encoder a
      Nothing -> Json.jsonNull
  }

-- | A schema for a closed set of string-tagged values. Encoding
-- | uses the supplied `toTag`; decoding looks the tag up in the
-- | table.
enum :: forall a. Eq a => Array (Tuple String a) -> (a -> String) -> Schema a
enum table toTag = Schema
  { decoder: \j -> case Json.toString j of
      Nothing -> Left (TypeMismatch { expected: "string", got: tag j })
      Just s -> case Foldable.find (\(Tuple k _) -> k == s) table of
        Nothing -> Left (RefinementFailed ("unknown tag: " <> show s))
        Just (Tuple _ a) -> Right a
  , encoder: \a -> Json.fromString (toTag a)
  }

-- | An applicative builder for record schemas. Type parameters:
-- |
-- |   * `r` is the record type being encoded.
-- |   * `a` is the value the builder produces while decoding.
-- |
-- | When `r ~ a` the builder describes a record schema directly;
-- | use `recordOf` to seal it.
newtype RecordSchema r a = RecordSchema
  { decoder :: Object Json -> Either DecodeError a
  , encoder :: r -> Array (Tuple String Json)
  }

instance functorRecordSchema :: Functor (RecordSchema r) where
  map f (RecordSchema rs) = RecordSchema
    { decoder: map f <<< rs.decoder
    , encoder: rs.encoder
    }

instance applyRecordSchema :: Apply (RecordSchema r) where
  apply (RecordSchema rf) (RecordSchema ra) = RecordSchema
    { decoder: \o -> rf.decoder o <*> ra.decoder o
    , encoder: \r -> rf.encoder r <> ra.encoder r
    }

instance applicativeRecordSchema :: Applicative (RecordSchema r) where
  pure a = RecordSchema
    { decoder: \_ -> Right a
    , encoder: \_ -> []
    }

-- | Declare a required field. Errors decoding the field are
-- | wrapped in `InvalidField`; a missing key produces
-- | `MissingField`.
field
  :: forall r a
   . String
  -> (r -> a)
  -> Schema a
  -> RecordSchema r a
field key getter (Schema s) = RecordSchema
  { decoder: \o -> case Object.lookup key o of
      Nothing -> Left (MissingField key)
      Just j -> lmap (InvalidField key) (s.decoder j)
  , encoder: \r -> [ Tuple key (s.encoder (getter r)) ]
  }

-- | Declare an optional field. A missing key decodes to
-- | `Nothing`; `Nothing` encodes by omitting the key. To accept
-- | an explicit JSON `null` as `Nothing`, pair this with
-- | `nullable`.
fieldOpt
  :: forall r a
   . String
  -> (r -> Maybe a)
  -> Schema a
  -> RecordSchema r (Maybe a)
fieldOpt key getter (Schema s) = RecordSchema
  { decoder: \o -> case Object.lookup key o of
      Nothing -> Right Nothing
      Just j -> map Just (lmap (InvalidField key) (s.decoder j))
  , encoder: \r -> case getter r of
      Nothing -> []
      Just a -> [ Tuple key (s.encoder a) ]
  }

-- | Like `field`, but supplies a fallback when the key is
-- | missing. The default value is *not* re-emitted on encode;
-- | encoding always writes the actual record value.
fieldDefault
  :: forall r a
   . String
  -> (r -> a)
  -> a
  -> Schema a
  -> RecordSchema r a
fieldDefault key getter def (Schema s) = RecordSchema
  { decoder: \o -> case Object.lookup key o of
      Nothing -> Right def
      Just j -> lmap (InvalidField key) (s.decoder j)
  , encoder: \r -> [ Tuple key (s.encoder (getter r)) ]
  }

-- | Seal a record builder into a `Schema`. The result decodes
-- | objects (failing with `TypeMismatch` on any other JSON kind)
-- | and encodes records as the concatenation of all declared
-- | field encoders.
recordOf :: forall a. RecordSchema a a -> Schema a
recordOf (RecordSchema rs) = Schema
  { decoder: \j -> case Json.toObject j of
      Nothing -> Left (TypeMismatch { expected: "object", got: tag j })
      Just o -> rs.decoder o
  , encoder: \a -> Json.fromObject (Object.fromFoldable (rs.encoder a))
  }

-- | Render a `DecodeError` as a short human-readable path-and-
-- | reason string. Treat the rendering as advisory: structure may
-- | change between versions.
renderError :: DecodeError -> String
renderError = go ""
  where
  go path = case _ of
    TypeMismatch m ->
      "expected " <> m.expected <> " at " <> showPath path
        <> ", got "
        <> m.got
    MissingField k ->
      "missing field at " <> showPath (path <> "." <> k)
    InvalidField k inner -> go (path <> "." <> k) inner
    InvalidIndex i inner -> go (path <> "[" <> show i <> "]") inner
    UnionMismatch errs ->
      "no union branch matched at " <> showPath path
        <> ":\n  "
        <> intercalate "\n  " (map (go path) errs)
    RefinementFailed msg ->
      "refinement failed at " <> showPath path <> ": " <> msg
    ParseError msg -> "parse error: " <> msg

  showPath "" = "$"
  showPath p = "$" <> p
