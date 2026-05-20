-- | File-backed `Source`s for `RIO.Aff.Config`.
-- |
-- | Two adapters are provided:
-- |
-- | * `dotenvFileSource path` reads a `.env`-style file and returns
-- |   a `Source` whose lookups go against the parsed entries.
-- | * `jsonFileSource path` reads a JSON file, flattens nested
-- |   objects into `_`-joined keys, and returns a `Source`. The
-- |   shape matches the way `RIO.Aff.Config.nested` qualifies keys, so
-- |   the same descriptor works against `envSource`, `jsonFileSource`,
-- |   and `dotenvFileSource`.
-- |
-- | Both functions perform file I/O in `Aff` and throw via the
-- | underlying `Aff` error channel on read failure, parse failure,
-- | or unsupported JSON shape. The pure parsers `parseDotenv` and
-- | `flattenJson` are exposed separately so tests and in-memory
-- | callers can avoid the file step.
-- |
-- | ```purescript
-- | import RIO.Aff.Config (load)
-- | import RIO.Aff.Config.File (jsonFileSource)
-- |
-- | main = launchAff_ do
-- |   src <- jsonFileSource "config.json"
-- |   runRIO' (load (Proxy :: _ "config") src appConfig program)
-- | ```
module RIO.Aff.Config.File
  ( dotenvFileSource
  , jsonFileSource
  , parseDotenv
  , flattenJson
  , DotenvError(..)
  , JsonShapeError(..)
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, isNull)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString) as Number
import Data.String (joinWith, trim) as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error) as Exception
import Foreign.Object as FObject
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile) as FS

import RIO.Aff.Config (Source, mkSource)

-- | A failure encountered while parsing a `.env` line. Line numbers
-- | are 1-based and refer to the original file.
data DotenvError = DotenvError Int String

derive instance eqDotenvError :: Eq DotenvError

instance showDotenvError :: Show DotenvError where
  show (DotenvError ln msg) =
    "DotenvError line " <> show ln <> ": " <> msg

-- | A failure encountered while flattening a JSON value into a
-- | `Source`-shaped map. Carries the path that was being walked
-- | when the shape problem was discovered.
data JsonShapeError = JsonShapeError (Array String) String

derive instance eqJsonShapeError :: Eq JsonShapeError

instance showJsonShapeError :: Show JsonShapeError where
  show (JsonShapeError path msg) =
    "JsonShapeError "
      <> show (String.joinWith "." path)
      <> ": "
      <> msg

-- | Parse a `.env`-style document. Accepts:
-- |
-- | * `KEY=value`
-- | * `export KEY=value`
-- | * `KEY="quoted value with spaces"` with `\n`, `\t`, `\r`,
-- |   `\\`, `\"` escapes
-- | * `KEY='single-quoted'` with no escape processing
-- | * `# comment lines` and blank lines
-- |
-- | Comments after a value (`KEY=value # note`) are stripped only
-- | when the value is not quoted; inside quotes, `#` is literal.
-- | Multi-line values are not supported.
parseDotenv :: String -> Either DotenvError (Map String String)
parseDotenv input =
  let
    rawLines =
      Array.mapWithIndex Tuple
        (Array.fromFoldable (split '\n' input))
    parseLine acc (Tuple ix line) = do
      m <- acc
      case parseDotenvLine line of
        Right Nothing -> Right m
        Right (Just (Tuple k v)) -> Right (Map.insert k v m)
        Left msg -> Left (DotenvError (ix + 1) msg)
  in
    Array.foldl parseLine (Right Map.empty) rawLines

split :: Char -> String -> Array String
split sep s =
  let
    go acc current chars = case Array.uncons chars of
      Nothing ->
        Array.snoc acc (CodeUnits.fromCharArray (Array.reverse current))
      Just { head, tail } ->
        if head == sep then
          go (Array.snoc acc (CodeUnits.fromCharArray (Array.reverse current))) [] tail
        else
          go acc (Array.cons head current) tail
  in
    go [] [] (CodeUnits.toCharArray s)

-- A single non-multi-line entry. `Right Nothing` means "ignored
-- line" (blank or comment).
parseDotenvLine :: String -> Either String (Maybe (Tuple String String))
parseDotenvLine raw =
  let
    trimmed = String.trim raw
  in
    case CodeUnits.charAt 0 trimmed of
      Nothing -> Right Nothing
      Just '#' -> Right Nothing
      _ -> parseEntry (stripExport trimmed)

stripExport :: String -> String
stripExport s =
  case stripPrefix "export " s of
    Just rest -> String.trim rest
    Nothing -> s

stripPrefix :: String -> String -> Maybe String
stripPrefix p s =
  let
    pc = CodeUnits.toCharArray p
    sc = CodeUnits.toCharArray s
  in
    case Array.take (Array.length pc) sc == pc of
      true -> Just (CodeUnits.fromCharArray (Array.drop (Array.length pc) sc))
      false -> Nothing

parseEntry :: String -> Either String (Maybe (Tuple String String))
parseEntry s = case indexOfChar '=' (CodeUnits.toCharArray s) of
  Nothing -> Left ("expected `=` in entry: " <> s)
  Just idx ->
    let
      chars = CodeUnits.toCharArray s
      keyChars = Array.take idx chars
      valChars = Array.drop (idx + 1) chars
      key = String.trim (CodeUnits.fromCharArray keyChars)
    in
      if key == "" then Left "empty key"
      else do
        value <- parseValue (CodeUnits.fromCharArray valChars)
        Right (Just (Tuple key value))

indexOfChar :: Char -> Array Char -> Maybe Int
indexOfChar c arr =
  let
    go i = case Array.index arr i of
      Nothing -> Nothing
      Just ch ->
        if ch == c then Just i
        else go (i + 1)
  in
    go 0

parseValue :: String -> Either String String
parseValue raw =
  let
    leftTrimmed = trimLeft raw
  in
    case CodeUnits.charAt 0 leftTrimmed of
      Just '"' -> parseDoubleQuoted (CodeUnits.drop 1 leftTrimmed)
      Just '\'' -> parseSingleQuoted (CodeUnits.drop 1 leftTrimmed)
      _ -> Right (String.trim (stripTrailingComment leftTrimmed))

trimLeft :: String -> String
trimLeft s =
  let
    chars = CodeUnits.toCharArray s
    dropped = Array.dropWhile (\c -> c == ' ' || c == '\t') chars
  in
    CodeUnits.fromCharArray dropped

stripTrailingComment :: String -> String
stripTrailingComment s =
  case findUnquotedHash (CodeUnits.toCharArray s) of
    Nothing -> s
    Just idx ->
      CodeUnits.fromCharArray
        (Array.take idx (CodeUnits.toCharArray s))

findUnquotedHash :: Array Char -> Maybe Int
findUnquotedHash arr =
  let
    go i = case Array.index arr i of
      Nothing -> Nothing
      Just '#' ->
        case Array.index arr (i - 1) of
          Just ' ' -> Just i
          Just '\t' -> Just i
          Nothing -> Just i
          _ -> go (i + 1)
      Just _ -> go (i + 1)
  in
    go 0

parseDoubleQuoted :: String -> Either String String
parseDoubleQuoted s = go (CodeUnits.toCharArray s) []
  where
  go chars acc = case Array.uncons chars of
    Nothing -> Left "unterminated double-quoted value"
    Just { head: '"', tail: _ } ->
      Right (CodeUnits.fromCharArray (Array.reverse acc))
    Just { head: '\\', tail } ->
      case Array.uncons tail of
        Nothing -> Left "trailing backslash in double-quoted value"
        Just { head: esc, tail: rest } ->
          go rest (Array.cons (escape esc) acc)
    Just { head, tail } -> go tail (Array.cons head acc)

  escape c = case c of
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '"' -> '"'
    '\\' -> '\\'
    other -> other

parseSingleQuoted :: String -> Either String String
parseSingleQuoted s = go (CodeUnits.toCharArray s) []
  where
  go chars acc = case Array.uncons chars of
    Nothing -> Left "unterminated single-quoted value"
    Just { head: '\'', tail: _ } ->
      Right (CodeUnits.fromCharArray (Array.reverse acc))
    Just { head, tail } -> go tail (Array.cons head acc)

-- | Read a `.env` file and return a `Source`. Throws via `Aff` on
-- | read or parse failure.
dotenvFileSource :: String -> Aff Source
dotenvFileSource path = do
  buf <- FS.readFile path
  text <- liftEffect (Buffer.toString UTF8 buf)
  case parseDotenv text of
    Left err -> Aff.throwError (Exception.error (show err))
    Right m -> pure (mkSource (\k -> Map.lookup k m))

-- | Flatten a JSON value into a `Map String String` keyed by
-- | `_`-joined paths. Strings round-trip as-is; numbers and
-- | booleans are rendered to their canonical string forms. Nulls
-- | and missing keys are not present in the map (so `optional` and
-- | `withDefault` recover them). Arrays are rejected with
-- | `JsonShapeError` because the `Source` model is one string per
-- | key; if you need array values, store them as JSON strings.
flattenJson :: Json -> Either JsonShapeError (Map String String)
flattenJson root = caseJson
  ( \_ ->
      -- top-level null → empty map; nested null is dropped per the
      -- `optional` story
      Right Map.empty
  )
  (\_ -> Left (JsonShapeError [] "top-level value must be an object"))
  (\_ -> Left (JsonShapeError [] "top-level value must be an object"))
  (\_ -> Left (JsonShapeError [] "top-level value must be an object"))
  ( \_ ->
      Left (JsonShapeError [] "top-level value must be an object")
  )
  (\obj -> flattenObject [] obj)
  root

flattenObject
  :: Array String
  -> FObject.Object Json
  -> Either JsonShapeError (Map String String)
flattenObject path obj =
  let
    step (Tuple k v) acc = do
      m <- acc
      flattenAt (Array.snoc path k) v >>= \inner ->
        Right (Map.union inner m)
  in
    foldr step (Right Map.empty)
      (FObject.toUnfoldable obj :: Array (Tuple String Json))

flattenAt
  :: Array String
  -> Json
  -> Either JsonShapeError (Map String String)
flattenAt path v
  | isNull v = Right Map.empty
  | otherwise = caseJson
      (\_ -> Right Map.empty)
      ( \b ->
          Right (Map.singleton (qualify path) (if b then "true" else "false"))
      )
      ( \n ->
          Right (Map.singleton (qualify path) (Number.toString n))
      )
      (\s -> Right (Map.singleton (qualify path) s))
      ( \_ ->
          Left
            (JsonShapeError path "array values are not supported")
      )
      (\obj -> flattenObject path obj)
      v

qualify :: Array String -> String
qualify path = String.joinWith "_" path

-- | Read a JSON file and return a `Source`. Throws via `Aff` on
-- | read, JSON parse, or shape errors. See `flattenJson` for the
-- | flattening rules.
jsonFileSource :: String -> Aff Source
jsonFileSource path = do
  buf <- FS.readFile path
  text <- liftEffect (Buffer.toString UTF8 buf)
  case jsonParser text of
    Left err ->
      Aff.throwError (Exception.error ("JSON parse error: " <> err))
    Right json -> case flattenJson json of
      Left err -> Aff.throwError (Exception.error (show err))
      Right m -> pure (mkSource (\k -> Map.lookup k m))
