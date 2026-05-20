-- | RIO-flavoured wrappers around `Node.Http2.Settings`.
module RIO.Fiber.Node.HTTP2.Settings
  ( module Exports
  , getDefaultSettings
  , getPackedSettings
  , getUnpackedSettings
  ) where

import Node.Buffer (Buffer)
import Node.Http2.Settings (defaultSettings) as Exports
import Node.Http2.Settings as S
import Node.Http2.Types (Settings)

import RIO.Fiber.Core (RIO, liftEffect)

-- | The settings Node would send by default.
getDefaultSettings :: forall r e. RIO r e Settings
getDefaultSettings = liftEffect S.getDefaultSettings

-- | Encode a `Settings` record as a SETTINGS frame buffer.
getPackedSettings :: forall r e. Settings -> RIO r e Buffer
getPackedSettings s = liftEffect (S.getPackedSettings s)

-- | Decode a SETTINGS frame buffer.
getUnpackedSettings :: forall r e. Buffer -> RIO r e Settings
getUnpackedSettings b = liftEffect (S.getUnpackedSettings b)
