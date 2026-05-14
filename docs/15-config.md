# Configuration

`RIO.Config` is a typed configuration layer. A `Config a` is a
value-level description of *how to read a typed `a` out of an
untyped source of strings* (process env, a JSON blob, a `Map`).
Primitive descriptors read one key; combinators decorate them
with structure; `Applicative` composes them into a record.

The interface is patterned after Effect-TS's `Config` and ZIO's
`Config`. The headline feature is that errors collect rather
than short-circuit: if `PORT` is unparseable and `DATABASE_URL`
is missing, one load reports both.

This doc covers:

1. The core type and its primitives (`string`, `int`,
   `boolean`, `secret`).
2. Combinators (`optional`, `withDefault`, `nested`,
   `Applicative` composition).
3. Sources (`envSource`, `mapSource`, `mkSource`, plus the
   file-backed sources in `rio-config-file`).
4. The `Secret` type and how redaction works.
5. Running a load (`load`, `ConfigError`, `prettyConfigError`).
6. Refreshable configs (`RIO.Config.Rotating`).

The source is `src/RIO/Config.purs`; the file-backed sources
live in the `rio-config-file` adapter package.

## The core type

```purescript
newtype Config a
```

A `Config a` is opaque: think of it as "a recipe that, given a
`Source` and a namespace path, either yields an `a` or
accumulates one or more errors." You build values with the
primitives below; you run them with `load`.

```purescript
type AppConfig =
  { port :: Int
  , dbUrl :: String
  , debug :: Boolean
  , apiKey :: Secret
  }

appConfig :: Config AppConfig
appConfig = { port: _, dbUrl: _, debug: _, apiKey: _ }
  <$> withDefault 8080 (int "PORT")
  <*> string "DATABASE_URL"
  <*> withDefault false (boolean "DEBUG")
  <*> secret "API_KEY"
```

Reading top-to-bottom: "PORT is an optional Int with default
8080; DATABASE_URL is a required String; DEBUG is an optional
Boolean with default false; API_KEY is a required Secret."

## Primitives

```purescript
string  :: String -> Config String
int     :: String -> Config Int
boolean :: String -> Config Boolean
secret  :: String -> Config Secret
```

Every primitive reads a single key. `int` accepts decimal
integers (`fromString`); `boolean` is case-insensitive and
accepts `true`/`false`, `yes`/`no`, `on`/`off`, `1`/`0`.
`secret` is identical to `string` except its value is wrapped
in the redacting `Secret` newtype.

Missing keys become `MissingKey`. Bad parses become
`ParseError` with the failing value embedded.

## Combinators

### `optional`

Soften a descriptor: a `MissingKey` failure becomes
`Right Nothing`. Other failures (most notably `ParseError`)
still propagate:

```purescript
optional :: forall a. Config a -> Config (Maybe a)
```

```purescript
-- present-but-unparseable still fails
maybePort :: Config (Maybe Int)
maybePort = optional (int "PORT")
```

### `withDefault`

Like `optional`, but supply a default instead of `Nothing`:

```purescript
withDefault :: forall a. a -> Config a -> Config a
```

`ParseError` still propagates; a default isn't a "fall back
if anything is wrong", it's a "fall back if the key isn't set
at all".

### `nested`

Run a descriptor under a namespace prefix. Inner key `K` is
looked up as `PREFIX_K`:

```purescript
nested :: forall a. String -> Config a -> Config a
```

```purescript
dbConfig :: Config DbConfig
dbConfig = nested "DB" $ { url: _, pool: _ }
  <$> string "URL"      -- read as DB_URL
  <*> int "POOL_SIZE"   -- read as DB_POOL_SIZE
```

Multiple `nested` layers stack: `nested "APP" (nested "DB"
(string "URL"))` reads `APP_DB_URL`.

### Applicative composition

`Config` is an `Applicative`. `<*>` runs both descriptors and
accumulates *both* failures if both fail, via a `Multi` node
in the resulting `ConfigError`. This is the headline behaviour:

```purescript
-- If PORT is unparseable AND DATABASE_URL is missing, the
-- load reports both. With Monad you'd only see the first.
```

There is intentionally no `Monad` instance: the error-accumulation
behavior depends on running every branch, which `>>=` cannot do
because the second branch depends on the first's result.

## Sources

A `Source` is an opaque key-to-string lookup:

```purescript
newtype Source

mkSource   :: (String -> Maybe String) -> Source
envSource  :: Effect Source                 -- live process env snapshot
mapSource  :: Map String String -> Source   -- pure, for tests
```

`envSource` snapshots the env in `Effect` so the read happens
at a well-defined point (typically startup); later mutations to
`process.env` are not seen. `mapSource` is the test entry
point; `mkSource` is the escape hatch for arbitrary lookups.

The `rio-config-file` adapter package ships two more sources:

```purescript
dotenvFileSource :: String -> Aff Source
jsonFileSource   :: String -> Aff Source
```

`dotenvFileSource` reads a `.env`-style file, supporting
`KEY=value`, `export KEY=value`, double- and single-quoted
values, comments, blank lines, and trailing comments outside
quotes. Parse errors surface with 1-based line numbers.

`jsonFileSource` reads a JSON file and flattens nested objects
into `_`-joined keys, matching the way `nested` qualifies keys.
The same `Config` descriptor works against env, dotenv, and
JSON sources without modification:

```purescript
-- All three loads use the same `appConfig` descriptor.
src1 <- liftEffect envSource
src2 <- liftAff (dotenvFileSource ".env")
src3 <- liftAff (jsonFileSource "config.json")
```

A `Source` is just a function; you can build one over a
process-env snapshot at startup, then overlay a file with
`mkSource` for development convenience.

## Secrets

```purescript
newtype Secret
unSecret :: Secret -> String
```

The `Show` instance for `Secret` renders `<redacted>`, so an
accidental `show` or log emission cannot leak the value.
`unSecret` is the explicit-on-purpose escape hatch: it shows up
in code review and grep, which is the point.

The `Eq` instance compares the underlying string; equality for
secrets is occasionally useful (rotation checks, for instance)
and the comparison itself doesn't reveal the value.

## Running a load

```purescript
load
  :: forall sym r e e' a
   . IsSymbol sym
  => Cons sym ConfigError e' e
  => Proxy sym
  -> Source
  -> Config a
  -> RIO r e a

data ConfigError
  = MissingKey Path String
  | ParseError Path String String
  | Multi (NonEmptyList ConfigError)

prettyConfigError :: ConfigError -> String
```

`load` runs the descriptor and either returns the value or
raises `ConfigError` on the user-chosen tag. The error row
constraint says "the row must contain a `sym :: ConfigError`
label"; the caller picks the tag.

```purescript
program
  :: forall r
   . RIO r (config :: ConfigError) AppConfig
program = do
  src <- liftEffect (liftAff envSource)
  load (Proxy :: _ "config") src appConfig
```

`prettyConfigError` renders the error as a multi-line summary
suitable for printing to stderr at startup. Sample output:

```
Configuration errors:
  - PORT: not an integer: abc
  - DATABASE_URL: missing
```

## Refreshable configs

For values that change at runtime (most commonly rotating
secrets), `RIO.Config.Rotating` provides a refreshable cell:

```purescript
newtype Rotating a

newRotating   :: forall a. a -> Effect (Rotating a)
readRotating  :: forall r e a. Rotating a -> RIO r e a
writeRotating :: forall r e a. Rotating a -> a -> RIO r e Unit

withRotation
  :: forall r e a
   . RIO r e a
  -> RIO r e (Tuple (Rotating a) (RIO r e Unit))
```

`newRotating` allocates a cell; `readRotating` / `writeRotating`
are atomic read / write primitives. `withRotation` is the
common shape: it runs the loader once to populate the cell
and returns a `refresh` action that re-runs the loader and
overwrites the cell:

```purescript
Tuple secretCell refresh <- withRotation loadApiSecret
-- ...later, on a signal or timer:
refresh
```

The module imposes no rotation policy: polling, signal
handling, or any other trigger is the caller's call. The
service is just an atomic cell with a built-in loader hook.

## Comparison with ZIO / Effect-TS

| Concept                | ZIO                       | Effect-TS                 | `purescript-rio`       |
| ---------------------- | ------------------------- | ------------------------- | ---------------------- |
| Descriptor type        | `Config[A]`               | `Config<A>`               | `Config a`             |
| Primitive read         | `Config.string` / `int`   | `Config.string` / `int`   | `string` / `int` / ... |
| Optional               | `Config.optional`         | `Config.option`           | `optional`             |
| Default                | `Config.withDefault`      | `Config.withDefault`      | `withDefault`          |
| Namespace              | `Config.nested`           | `Config.nested`           | `nested`               |
| Source from env        | `ConfigProvider.fromEnv`  | `ConfigProvider.fromEnv`  | `envSource`            |
| Source from file       | `ConfigProvider.fromJson` | `ConfigProvider.fromJson` | `jsonFileSource` (in `rio-config-file`) |
| Redacted string        | `Config.secret`           | `Config.secret`           | `secret` / `Secret`    |
| Error accumulation     | `Multi`                   | `Cause.Parallel`          | `Multi`                |
| Refreshable cell       | (manual via `Ref`)        | (manual via `Ref`)        | `RIO.Config.Rotating`  |

The error-accumulation behavior matches Effect-TS exactly; ZIO
1.x used a slightly different shape that's since aligned with
the "collect everything" default in 2.x.

## Pointers

- Source: [`src/RIO/Config.purs`](../src/RIO/Config.purs).
- Refreshable cells:
  [`src/RIO/Config/Rotating.purs`](../src/RIO/Config/Rotating.purs).
- File-backed sources:
  [`rio-config-file/`](../rio-config-file/).
- Spec coverage:
  [`test/Test/RIO/ConfigSpec.purs`](../test/Test/RIO/ConfigSpec.purs),
  [`test/Test/RIO/Config/RotatingSpec.purs`](../test/Test/RIO/Config/RotatingSpec.purs),
  and the in-package tests under
  [`rio-config-file/test/`](../rio-config-file/test/).
