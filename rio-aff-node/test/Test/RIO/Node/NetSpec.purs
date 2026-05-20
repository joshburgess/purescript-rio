module Test.RIO.Aff.Node.NetSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.EventEmitter (once) as NE
import Node.Net.Server (connectionH, listenTcp, listeningH) as NSrv
import Node.Net.Socket (toDuplex) as NSock
import Node.Stream (pipe) as NStream
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Node.Net (IpFamily(..), isIP, isIPv4, isIPv6) as Net
import RIO.Aff.Node.Net.Server as Server
import RIO.Aff.Node.Net.Socket as Socket
import RIO.Aff.Node.Net.SocketAddress as SA
import RIO.Aff.Node.Stream as Stream

runN :: forall a. RIO () () a -> Aff a
runN = runRIO'

spec :: Spec Unit
spec = describe "RIO.Aff.Node.Net" do
  describe "isIP family" do
    it "classifies a literal v4 address" do
      Net.isIPv4 "127.0.0.1" `shouldEqual` true
      Net.isIPv6 "127.0.0.1" `shouldEqual` false
      Net.isIP "127.0.0.1" `shouldEqual` Just Net.IPv4

    it "classifies a literal v6 address" do
      Net.isIPv4 "::1" `shouldEqual` false
      Net.isIPv6 "::1" `shouldEqual` true
      Net.isIP "::1" `shouldEqual` Just Net.IPv6

    it "returns Nothing for non-IP strings" do
      Net.isIP "not an ip" `shouldEqual` Nothing

  describe "SocketAddress" do
    it "newIpv4 round-trips its inputs through the accessors" do
      r <- runN do
        sa <- SA.newIpv4 { address: "127.0.0.1", port: 5555 }
        pure
          { addr: SA.address sa
          , fam: SA.family sa
          , port: SA.port sa
          }
      r.addr `shouldEqual` "127.0.0.1"
      r.fam `shouldEqual` Net.IPv4
      r.port `shouldEqual` 5555

    it "newIpv6 round-trips its inputs through the accessors" do
      r <- runN do
        sa <- SA.newIpv6
          { address: "::1", port: 6666, flowLabel: 99 }
        pure
          { addr: SA.address sa
          , fam: SA.family sa
          , port: SA.port sa
          }
      r.addr `shouldEqual` "::1"
      r.fam `shouldEqual` Net.IPv6
      r.port `shouldEqual` 6666

  describe "TCP server / socket round-trip" do
    it "a server bound to port 0 reports its bound port" do
      port <- do
        srv <- runN (Server.createTcpServer)
        portRef <- liftEffect (Ref.new 0)

        makeAff \done -> do
          remove <- srv # NE.once NSrv.listeningH do
            done (Right unit)
          NSrv.listenTcp srv { port: 0, host: "127.0.0.1" }
          pure (effectCanceler remove)

        addr <- runN (Server.addressTcp srv)
        case addr of
          Just a -> liftEffect (Ref.write a.port portRef)
          Nothing -> pure unit
        runN (Server.close srv)
        liftEffect (Ref.read portRef)
      (port > 0) `shouldEqual` true

    it "a client connects to the server and the server pipes data back" do
      received <- do
        srv <- runN Server.createTcpServer

        -- When the server gets a connection, pipe the socket
        -- back to itself so anything the client sends is echoed
        -- to the client.
        _ <- liftEffect $ srv # NE.once NSrv.connectionH \sock -> do
          let dup = NSock.toDuplex sock
          void (NStream.pipe dup dup)

        -- Start listening on an ephemeral port.
        makeAff \done -> do
          remove <- srv # NE.once NSrv.listeningH (done (Right unit))
          NSrv.listenTcp srv { port: 0, host: "127.0.0.1" }
          pure (effectCanceler remove)

        addr <- runN (Server.addressTcp srv)
        let
          port = case addr of
            Just a -> a.port
            Nothing -> 0

        -- Connect a client.
        sock <- runN
          (Socket.createConnectionTCP { port, host: "127.0.0.1" })
        let cdup = NSock.toDuplex sock
        _ <- runN (Stream.writeString cdup UTF8 "hello-echo")
        runN (Stream.end cdup)

        out <- runN (Stream.readableToStringUtf8 cdup)
        runN (Server.close srv)
        pure out
      received `shouldEqual` "hello-echo"
