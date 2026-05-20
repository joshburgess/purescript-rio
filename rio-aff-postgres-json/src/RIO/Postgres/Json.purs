-- | Argonaut-backed adapter for Postgres `json` / `jsonb` columns.
-- |
-- | Wrap a value in `JsonB` to make it bind to a `jsonb` parameter
-- | (`EncodeJson` is enough) or decode out of a `jsonb` column
-- | (`DecodeJson` is enough). The newtype only exists to anchor
-- | the type-class instances; in user code it's mostly transparent.
-- |
-- | The driver returns `jsonb` columns as raw JSON strings (see
-- | `Data.Postgres.modifyPgTypes`), so `JsonB`'s instances go
-- | through `String` on both sides: stringify on insert, parse on
-- | select. Decode failures surface as `ForeignError` and bubble
-- | up to whichever `PgError` tag the call site is using.
-- |
-- | ```purescript
-- | -- insert: pass a Json or any EncodeJson value
-- | _ <- execParams dbTag
-- |   "insert into events (payload) values ($1)"
-- |   (JsonB { kind: "login", userId: 42 })
-- |
-- | -- select: read back into Json or any DecodeJson value
-- | rows <- query dbTag
-- |   "select payload from events order by id"
-- | for_ (rows :: Array (JsonB Event)) \(JsonB e) ->
-- |   logInfo (show e.kind)
-- | ```
-- |
-- | `JsonB Json` round-trips an untyped `Data.Argonaut.Core.Json`
-- | value, useful when the schema is dynamic.
module RIO.Aff.Postgres.Json
  ( JsonB(..)
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign (ForeignError(..))

import Data.Postgres (class Deserialize, class Serialize, deserialize, serialize)

-- | A value that should round-trip through a Postgres `json` /
-- | `jsonb` column. The inner value is encoded via Argonaut on the
-- | way in and decoded via Argonaut on the way out.
newtype JsonB a = JsonB a

derive instance Newtype (JsonB a) _
derive newtype instance Show a => Show (JsonB a)
derive newtype instance Eq a => Eq (JsonB a)
derive newtype instance Ord a => Ord (JsonB a)

instance EncodeJson a => Serialize (JsonB a) where
  serialize = serialize <<< stringify <<< encodeJson <<< unwrap

instance DecodeJson a => Deserialize (JsonB a) where
  deserialize raw = do
    text <- deserialize @String raw
    parsed <- except
      ( lmap (\msg -> NEL.singleton (ForeignError msg)) (jsonParser text)
      )
    case decodeJson parsed of
      Right a -> pure (wrap a)
      Left err -> throwError
        ( NEL.singleton (ForeignError (printJsonDecodeError err))
        )
