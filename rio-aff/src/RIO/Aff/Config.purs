-- | Typed configuration descriptors.
-- |
-- | Inspired by Effect-TS's `Config` and ZIO's `Config`: a `Config a`
-- | is a value-level description of how to read a typed `a` out of
-- | some untyped source of strings (process env, a JSON blob,
-- | a `Map`). Primitive descriptors (`string`, `int`, `boolean`,
-- | `secret`) read one key; combinators (`optional`, `withDefault`,
-- | `nested`) decorate that with structure, and `Applicative`
-- | lets you combine several descriptors into a record-shaped
-- | result, accumulating every missing or unparseable key into a
-- | single error tree.
-- |
-- | ```purescript
-- | type AppConfig =
-- |   { port :: Int
-- |   , dbUrl :: String
-- |   , debug :: Boolean
-- |   , apiKey :: Secret
-- |   }
-- |
-- | appConfig :: Config AppConfig
-- | appConfig = { port: _, dbUrl: _, debug: _, apiKey: _ }
-- |   <$> withDefault 8080 (int "PORT")
-- |   <*> string "DATABASE_URL"
-- |   <*> withDefault false (boolean "DEBUG")
-- |   <*> secret "API_KEY"
-- |
-- | main = launchAff_ do
-- |   src <- liftEffect envSource
-- |   runRIO' (load (Proxy :: _ "config") src appConfig)
-- | ```
-- |
-- | Errors collect rather than short-circuit: if both `PORT` is
-- | unparseable and `DATABASE_URL` is missing, the load reports both.
module RIO.Aff.Config
  ( Config
  , ConfigError(..)
  , Path
  , Secret
  , unSecret
  , string
  , int
  , boolean
  , secret
  , optional
  , withDefault
  , nested
  , Source
  , mkSource
  , envSource
  , mapSource
  , load
  , prettyConfigError
  ) where

import Prelude

import Data.Array (intercalate, snoc) as Array
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (toLower) as String
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Foreign.Object as Object
import Node.Process (getEnv)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

import RIO.Aff.Core (RIO)
import RIO.Aff.Error (fail)

-- | A namespace path, prepended to keys by `nested`.
type Path = Array String

-- | One failure during a config load. `Multi` collects parallel
-- | failures so a single load reports every problem at once.
data ConfigError
  = MissingKey Path String
  | ParseError Path String String
  | Multi (NEL.NonEmptyList ConfigError)

derive instance eqConfigError :: Eq ConfigError

instance showConfigError :: Show ConfigError where
  show = case _ of
    MissingKey p k ->
      "MissingKey " <> show (renderPath p k)
    ParseError p k msg ->
      "ParseError " <> show (renderPath p k) <> " (" <> msg <> ")"
    Multi es ->
      "Multi " <> show (NEL.toUnfoldable es :: Array ConfigError)

renderPath :: Path -> String -> String
renderPath p k =
  let
    parts = Array.snoc p k
  in
    Array.intercalate "." parts

-- | A string wrapper whose `Show` instance redacts the contents.
-- | Use for credentials so an accidental `show` or log emission
-- | does not leak the value.
newtype Secret = Secret String

instance showSecret :: Show Secret where
  show _ = "<redacted>"

derive instance eqSecret :: Eq Secret

-- | Unwrap a `Secret` when you actually need to pass it to a
-- | network client or driver. Intentionally explicit so it shows
-- | up in code review.
unSecret :: Secret -> String
unSecret (Secret s) = s

-- | An opaque key-to-string lookup. Construct with `envSource`
-- | (live process env) or `mapSource` (pure, for tests).
newtype Source = Source (String -> Maybe String)

-- | Build a `Source` from an arbitrary lookup function.
mkSource :: (String -> Maybe String) -> Source
mkSource = Source

-- | Snapshot the process environment as a `Source`. Returned in
-- | `Effect` so the snapshot happens at a well-defined point
-- | (typically startup); subsequent mutations to `process.env`
-- | are not seen.
envSource :: Effect Source
envSource = do
  obj <- getEnv
  pure (Source (\k -> Object.lookup k obj))

-- | Build a `Source` from a pure `Map`. Useful in tests.
mapSource :: Map String String -> Source
mapSource m = Source (\k -> Map.lookup k m)

-- | A descriptor that, given a `Source` and a namespace path,
-- | either produces a value or accumulates one or more errors.
newtype Config a = Config (Source -> Path -> Either ConfigError a)

unConfig
  :: forall a
   . Config a
  -> Source
  -> Path
  -> Either ConfigError a
unConfig (Config f) = f

instance functorConfig :: Functor Config where
  map f (Config k) = Config \src path -> map f (k src path)

instance applyConfig :: Apply Config where
  apply (Config kf) (Config ka) = Config \src path ->
    case kf src path, ka src path of
      Right f, Right a -> Right (f a)
      Left e1, Left e2 -> Left (combine e1 e2)
      Left e, _ -> Left e
      _, Left e -> Left e

instance applicativeConfig :: Applicative Config where
  pure a = Config \_ _ -> Right a

-- | Smush two errors into one. Flattens `Multi` so nesting stays
-- | shallow regardless of how the descriptor tree was assembled.
combine :: ConfigError -> ConfigError -> ConfigError
combine a b =
  let
    toNel = case _ of
      Multi xs -> xs
      e -> NEL.singleton e
  in
    Multi (toNel a <> toNel b)

-- A primitive that reads `key` and runs `parse` over the raw
-- string. Missing keys become `MissingKey`; failed parses become
-- `ParseError`.
primitive
  :: forall a
   . String
  -> (String -> Either String a)
  -> Config a
primitive key parse = Config \(Source lookup) path ->
  case lookup (qualify path key) of
    Nothing -> Left (MissingKey path key)
    Just raw -> case parse raw of
      Left msg -> Left (ParseError path key msg)
      Right a -> Right a

qualify :: Path -> String -> String
qualify path key = case path of
  [] -> key
  _ -> Array.intercalate "_" path <> "_" <> key

-- | Read a required `String` key.
string :: String -> Config String
string key = primitive key Right

-- | Read a required `Int` key. The parse uses `Data.Int.fromString`,
-- | which accepts decimal integers and rejects everything else.
int :: String -> Config Int
int key = primitive key \raw ->
  case Int.fromString raw of
    Just n -> Right n
    Nothing -> Left ("not an integer: " <> raw)

-- | Read a required `Boolean` key. Accepts `true`/`false`,
-- | `yes`/`no`, `on`/`off`, `1`/`0`, case-insensitive.
boolean :: String -> Config Boolean
boolean key = primitive key \raw ->
  case String.toLower raw of
    "true" -> Right true
    "yes" -> Right true
    "on" -> Right true
    "1" -> Right true
    "false" -> Right false
    "no" -> Right false
    "off" -> Right false
    "0" -> Right false
    _ -> Left ("not a boolean: " <> raw)

-- | Read a required `Secret` key. The value is identical to
-- | `string key` but wrapped so its `Show` instance redacts it.
secret :: String -> Config Secret
secret key = primitive key (Right <<< Secret)

-- | Soften a descriptor: a `MissingKey` failure becomes a
-- | successful `Nothing`. `ParseError` and other failures still
-- | propagate.
optional :: forall a. Config a -> Config (Maybe a)
optional (Config k) = Config \src path ->
  case k src path of
    Right a -> Right (Just a)
    Left (MissingKey _ _) -> Right Nothing
    Left e -> Left e

-- | Supply a default for a missing key. `ParseError` and other
-- | failures still propagate.
withDefault :: forall a. a -> Config a -> Config a
withDefault def (Config k) = Config \src path ->
  case k src path of
    Right a -> Right a
    Left (MissingKey _ _) -> Right def
    Left e -> Left e

-- | Run a descriptor under a namespace prefix. Inner key `K` is
-- | looked up as `PREFIX_K`; nested `nested "DB" (string "URL")`
-- | looks up `DB_URL`.
nested :: forall a. String -> Config a -> Config a
nested prefix (Config k) = Config \src path ->
  k src (Array.snoc path prefix)

-- | Run a `Config` and surface the result in `RIO`. Failures are
-- | raised on the chosen error tag.
-- |
-- | ```purescript
-- | program :: RIO r (config :: ConfigError) AppConfig
-- | program = load (Proxy :: _ "config") src appConfig
-- | ```
load
  :: forall sym r e e' a
   . IsSymbol sym
  => Cons sym ConfigError e' e
  => Proxy sym
  -> Source
  -> Config a
  -> RIO r e a
load sym src cfg =
  case unConfig cfg src [] of
    Right a -> pure a
    Left err -> fail sym err

-- | Render a `ConfigError` as a multi-line, human-readable
-- | summary. Suitable for printing to stderr at startup.
prettyConfigError :: ConfigError -> String
prettyConfigError = case _ of
  MissingKey p k ->
    "missing required config key: " <> renderPath p k
  ParseError p k msg ->
    "could not parse config key " <> renderPath p k <> ": " <> msg
  Multi es ->
    "config failed to load:\n" <>
      Array.intercalate "\n"
        ( map (\e -> "  - " <> prettyConfigError e)
            (NEL.toUnfoldable es)
        )
