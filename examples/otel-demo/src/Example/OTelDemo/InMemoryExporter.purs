-- | Demo-only wiring around `@opentelemetry/sdk-trace-base`'s
-- | `InMemorySpanExporter`. This is not part of the `rio-aff-otel`
-- | public surface; it exists so the example can run end-to-end
-- | without a network exporter.
module Example.OTelDemo.InMemoryExporter
  ( ExportedSpan
  , installInMemoryExporter
  , readExportedSpans
  ) where

import Prelude

import Effect (Effect)

-- | A flattened view of an OTel exported span. The OTel SDK
-- | hands us a richer record (kind, events, links, hr-times);
-- | the demo trims it to just what we want to print.
type ExportedSpan =
  { name :: String
  , parentId :: String
  , status :: String
  , attributes :: Array { key :: String, value :: String }
  }

foreign import installInMemoryExporter :: String -> Effect Unit
foreign import readExportedSpans :: Effect (Array ExportedSpan)
