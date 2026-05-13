-- | JSON codecs for the wire types. `argonaut-codecs` provides
-- | `DecodeJson` / `EncodeJson` for records; we wrap the
-- | combinators in HTTPurple's `JsonEncoder` / `JsonDecoder`
-- | newtypes so the request/response helpers in `HTTPurple.Json`
-- | consume them.
module Example.TodoApi.Codecs
  ( CreateTodo
  , createTodoDecoder
  , decodeError
  , encodeTodo
  , encodeTodos
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Effect.Aff (Milliseconds(..))
import HTTPurple (JsonDecoder(..), JsonEncoder(..))

import Example.TodoApi.Services (Todo)

-- | The body shape accepted by `POST /todos`.
type CreateTodo = { title :: String }

-- | A `JsonDecoder` that runs argonaut's parse + decode and
-- | bubbles any error up as a `JsonDecodeError`.
createTodoDecoder :: JsonDecoder JsonDecodeError CreateTodo
createTodoDecoder = JsonDecoder \s -> do
  json <- parseJson s
  decodeJson json

-- | Render a `JsonDecodeError` as the 400 response body.
decodeError :: JsonDecodeError -> String
decodeError = printJsonDecodeError

-- | Single-todo encoder. The wire shape unpacks `createdAt` into
-- | a `Number` (milliseconds since epoch) for client-side
-- | parsing.
encodeTodo :: JsonEncoder Todo
encodeTodo = JsonEncoder (stringify <<< encodeJson <<< toWire)

-- | List encoder, same per-row shape as `encodeTodo`.
encodeTodos :: JsonEncoder (Array Todo)
encodeTodos = JsonEncoder (stringify <<< encodeJson <<< map toWire)

toWire
  :: Todo
  -> { id :: Int, title :: String, done :: Boolean, createdAt :: Number }
toWire t =
  let
    Milliseconds n = t.createdAt
  in
    { id: t.id
    , title: t.title
    , done: t.done
    , createdAt: n
    }
