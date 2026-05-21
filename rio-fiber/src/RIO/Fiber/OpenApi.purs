-- | Emit OpenAPI 3.1 documents from in-repo route declarations.
-- |
-- | `RIO.Fiber.OpenApi` is the data-only side of the API: a small set of
-- | record types describing an OpenAPI document (`OpenApiDoc`,
-- | `Info`, `Operation`, `Parameter`, ...), builders for the
-- | common case, and `emit :: OpenApiDoc -> Json` which renders
-- | the document to JSON exactly as an OpenAPI consumer expects.
-- |
-- | The module reuses `RIO.Fiber.Schema`'s `toJsonSchema` for body and
-- | parameter shapes, so a single `Schema a` value drives both
-- | wire decoding (`RIO.Fiber.Schema.decode`) and the OpenAPI fragment.
-- |
-- | This is a one-way emit: there is no consumer of OpenAPI JSON,
-- | no validation of the document, and no runtime cross-check
-- | that a server's actual handler returns what the spec
-- | promises. Pair it with a contract test if both ends need to
-- | stay in lock-step.
-- |
-- | ```purescript
-- | doc :: OpenApiDoc
-- | doc =
-- |   { info: { title: "Users API", version: "1.0.0", description: Nothing }
-- |   , servers: [ { url: "https://api.example.com", description: Nothing } ]
-- |   , paths:
-- |       [ { path: "/users"
-- |         , operations:
-- |             [ { method: GET
-- |               , summary: Just "List users"
-- |               , description: Nothing
-- |               , operationId: Just "listUsers"
-- |               , tags: [ "users" ]
-- |               , parameters: []
-- |               , requestBody: Nothing
-- |               , responses:
-- |                   [ { status: "200"
-- |                     , description: "ok"
-- |                     , content: Just
-- |                         { mediaType: "application/json"
-- |                         , schema: Schema.toJsonSchema (Schema.array userSchema)
-- |                         }
-- |                     }
-- |                   ]
-- |               }
-- |             ]
-- |         }
-- |       ]
-- |   , components: { schemas: [] }
-- |   }
-- |
-- | -- toplevel JSON suitable for writing to `openapi.json`
-- | docJson :: Json
-- | docJson = emit doc
-- | ```
module RIO.Fiber.OpenApi
  ( OpenApiDoc
  , Info
  , Server
  , PathItem
  , Operation
  , Parameter
  , ParameterIn(..)
  , RequestBodySpec
  , Response
  , Content
  , Components
  , defaultDoc
  , operation
  , pathParam
  , queryParam
  , headerParam
  , response
  , jsonContent
  , emit
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

import RIO.Fiber.HttpClient (Method)

-- | The root document.
type OpenApiDoc =
  { info :: Info
  , servers :: Array Server
  , paths :: Array PathItem
  , components :: Components
  }

-- | The `info` object: title, version, and optional description.
type Info =
  { title :: String
  , version :: String
  , description :: Maybe String
  }

-- | A `servers[]` entry.
type Server =
  { url :: String
  , description :: Maybe String
  }

-- | A `paths[]` entry: the URL pattern (e.g. `/users/{id}`) and
-- | the per-method operations declared on it.
type PathItem =
  { path :: String
  , operations :: Array Operation
  }

-- | One operation: method, OpenAPI metadata, parameters, body,
-- | responses.
type Operation =
  { method :: Method
  , summary :: Maybe String
  , description :: Maybe String
  , operationId :: Maybe String
  , tags :: Array String
  , parameters :: Array Parameter
  , requestBody :: Maybe RequestBodySpec
  , responses :: Array Response
  }

-- | A single parameter declaration.
type Parameter =
  { name :: String
  , in :: ParameterIn
  , required :: Boolean
  , description :: Maybe String
  , schema :: Json
  }

-- | Where a parameter is carried in the request.
data ParameterIn = InPath | InQuery | InHeader | InCookie

derive instance eqParameterIn :: Eq ParameterIn

instance showParameterIn :: Show ParameterIn where
  show = case _ of
    InPath -> "path"
    InQuery -> "query"
    InHeader -> "header"
    InCookie -> "cookie"

-- | Request body declaration.
type RequestBodySpec =
  { description :: Maybe String
  , required :: Boolean
  , content :: Content
  }

-- | A single response under the operation's `responses` map.
-- | `status` is the response key ("200", "404", "default", ...).
type Response =
  { status :: String
  , description :: String
  , content :: Maybe Content
  }

-- | A `content` body declaration: media type plus its schema.
type Content =
  { mediaType :: String
  , schema :: Json
  }

-- | The `components` section, currently only carrying named
-- | schemas. Extend in place if you need `securitySchemes` etc.
type Components =
  { schemas :: Array (Tuple String Json)
  }

-- | A reasonable default skeleton: just `info`, no paths, no
-- | servers, no components. Use as a starting point and fill in.
defaultDoc :: String -> String -> OpenApiDoc
defaultDoc title version =
  { info: { title, version, description: Nothing }
  , servers: []
  , paths: []
  , components: { schemas: [] }
  }

-- | Build a minimal `Operation` with just a method. Caller fills
-- | in the rest by record update.
operation :: Method -> Operation
operation method =
  { method
  , summary: Nothing
  , description: Nothing
  , operationId: Nothing
  , tags: []
  , parameters: []
  , requestBody: Nothing
  , responses: []
  }

-- | Build a path parameter (path params are always required).
pathParam :: String -> Json -> Parameter
pathParam name schema =
  { name
  , in: InPath
  , required: true
  , description: Nothing
  , schema
  }

-- | Build a query parameter. Defaults to not required; flip the
-- | `required` field by record update if needed.
queryParam :: String -> Json -> Parameter
queryParam name schema =
  { name
  , in: InQuery
  , required: false
  , description: Nothing
  , schema
  }

-- | Build a header parameter. Defaults to not required.
headerParam :: String -> Json -> Parameter
headerParam name schema =
  { name
  , in: InHeader
  , required: false
  , description: Nothing
  , schema
  }

-- | Build a `Response` with the supplied status, description,
-- | and content body.
response :: String -> String -> Maybe Content -> Response
response status description content =
  { status, description, content }

-- | Build a JSON `Content` block from a schema fragment.
jsonContent :: Json -> Content
jsonContent schema = { mediaType: "application/json", schema }

-- | Render the document to JSON.
emit :: OpenApiDoc -> Json
emit doc =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "openapi" (Json.fromString "3.1.0")
          , Tuple "info" (emitInfo doc.info)
          ]
            <> serversField doc.servers
            <> [ Tuple "paths" (emitPaths doc.paths) ]
            <> componentsField doc.components
        )
    )
  where
  serversField servers
    | Array.null servers = []
    | otherwise =
        [ Tuple "servers" (Json.fromArray (map emitServer servers)) ]

  componentsField components
    | Array.null components.schemas = []
    | otherwise =
        [ Tuple "components" (emitComponents components) ]

emitInfo :: Info -> Json
emitInfo info =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "title" (Json.fromString info.title)
          , Tuple "version" (Json.fromString info.version)
          ]
            <> maybeField "description" Json.fromString info.description
        )
    )

emitServer :: Server -> Json
emitServer server =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "url" (Json.fromString server.url) ]
            <> maybeField "description" Json.fromString server.description
        )
    )

emitPaths :: Array PathItem -> Json
emitPaths items =
  Json.fromObject
    ( Object.fromFoldable
        (map (\p -> Tuple p.path (emitPathItem p)) items)
    )

emitPathItem :: PathItem -> Json
emitPathItem item =
  Json.fromObject
    ( Object.fromFoldable
        (map operationEntry item.operations)
    )
  where
  operationEntry op = Tuple (methodKey op.method) (emitOperation op)

emitOperation :: Operation -> Json
emitOperation op =
  Json.fromObject
    ( Object.fromFoldable
        ( maybeField "summary" Json.fromString op.summary
            <> maybeField "description" Json.fromString op.description
            <> maybeField "operationId" Json.fromString op.operationId
            <> tagsField op.tags
            <> parametersField op.parameters
            <> requestBodyField op.requestBody
            <> [ Tuple "responses" (emitResponses op.responses) ]
        )
    )
  where
  tagsField tags
    | Array.null tags = []
    | otherwise =
        [ Tuple "tags" (Json.fromArray (map Json.fromString tags)) ]

  parametersField params
    | Array.null params = []
    | otherwise =
        [ Tuple "parameters" (Json.fromArray (map emitParameter params)) ]

  requestBodyField = case _ of
    Nothing -> []
    Just rb -> [ Tuple "requestBody" (emitRequestBody rb) ]

emitParameter :: Parameter -> Json
emitParameter p =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "name" (Json.fromString p.name)
          , Tuple "in" (Json.fromString (show p.in))
          , Tuple "required" (Json.fromBoolean p.required)
          , Tuple "schema" p.schema
          ]
            <> maybeField "description" Json.fromString p.description
        )
    )

emitRequestBody :: RequestBodySpec -> Json
emitRequestBody rb =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "required" (Json.fromBoolean rb.required)
          , Tuple "content" (emitContentMap rb.content)
          ]
            <> maybeField "description" Json.fromString rb.description
        )
    )

emitResponses :: Array Response -> Json
emitResponses rs =
  Json.fromObject
    ( Object.fromFoldable
        (map (\r -> Tuple r.status (emitResponse r)) rs)
    )

emitResponse :: Response -> Json
emitResponse r =
  Json.fromObject
    ( Object.fromFoldable
        ( [ Tuple "description" (Json.fromString r.description) ]
            <> contentField r.content
        )
    )
  where
  contentField = case _ of
    Nothing -> []
    Just c -> [ Tuple "content" (emitContentMap c) ]

emitContentMap :: Content -> Json
emitContentMap c =
  Json.fromObject
    ( Object.singleton c.mediaType
        ( Json.fromObject
            (Object.singleton "schema" c.schema)
        )
    )

emitComponents :: Components -> Json
emitComponents components =
  Json.fromObject
    ( Object.fromFoldable
        [ Tuple "schemas"
            ( Json.fromObject
                (Object.fromFoldable components.schemas)
            )
        ]
    )

methodKey :: Method -> String
methodKey = show >>> \s -> toLower s

-- Cheap ASCII lowercase, sufficient for the seven HTTP method
-- tags that `RIO.Fiber.HttpClient.Method` carries.
toLower :: String -> String
toLower = case _ of
  "GET" -> "get"
  "POST" -> "post"
  "PUT" -> "put"
  "PATCH" -> "patch"
  "DELETE" -> "delete"
  "HEAD" -> "head"
  "OPTIONS" -> "options"
  other -> other

maybeField
  :: forall a
   . String
  -> (a -> Json)
  -> Maybe a
  -> Array (Tuple String Json)
maybeField key f = maybe [] (\v -> [ Tuple key (f v) ])
