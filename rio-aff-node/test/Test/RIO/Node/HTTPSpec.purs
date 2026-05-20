module Test.RIO.Aff.Node.HTTPSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (once) as NE
import Node.HTTP.ClientRequest (responseH, toOutgoingMessage) as CRq
import Node.HTTP.IncomingMessage (toReadable) as IM
import Node.HTTP.OutgoingMessage (toWriteable) as OM
import Node.HTTP.Server (requestH, toNetServer) as HSrv
import Node.HTTP.ServerResponse (toOutgoingMessage) as SR
import Node.Net.Server (addressTcp, close, listenTcp, listeningH) as NSrv
import Node.Stream as NStream
import Node.Stream.Aff (readableToStringUtf8) as NStreamAff
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Node.HTTP as HTTP

runN :: forall a. RIO () () a -> Aff a
runN = runRIO'

spec :: Spec Unit
spec = describe "RIO.Aff.Node.HTTP" do
  describe "createServer + request round-trip" do
    it "a server responds to a GET and the client reads the body" do
      body <- do
        srv <- runN HTTP.createServer

        -- Wire up the request handler before listening.
        _ <- liftEffect $ srv # NE.once HSrv.requestH \_ res -> do
          let w = OM.toWriteable (SR.toOutgoingMessage res)
          _ <- NStream.writeString w UTF8 "ok-body"
          NStream.end w

        -- Listen on an ephemeral port and wait for the
        -- listening event.
        let net = HSrv.toNetServer srv
        makeAff \done -> do
          remove <- net # NE.once NSrv.listeningH (done (Right unit))
          NSrv.listenTcp net { port: 0, host: "127.0.0.1" }
          pure (effectCanceler remove)

        addr <- liftEffect (NSrv.addressTcp net)
        let
          port = case addr of
            Just a -> a.port
            Nothing -> 0

        cr <- runN
          ( HTTP.request' "http://127.0.0.1/"
              { host: "127.0.0.1"
              , port
              , path: "/"
              , method: "GET"
              }
          )
        liftEffect
          (NStream.end (OM.toWriteable (CRq.toOutgoingMessage cr)))

        imsg <- makeAff \done -> do
          remove <- cr # NE.once CRq.responseH \im -> done (Right im)
          pure (effectCanceler remove)

        out <- NStreamAff.readableToStringUtf8 (IM.toReadable imsg)
        liftEffect (NSrv.close net)
        pure out
      body `shouldEqual` "ok-body"
