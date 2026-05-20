module Test.RioFiberNode.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Fiber.Node.BufferSpec as BufferSpec
import Test.RIO.Fiber.Node.ChildProcessSpec as ChildProcessSpec
import Test.RIO.Fiber.Node.EventEmitterSpec as EventEmitterSpec
import Test.RIO.Fiber.Node.FileSystemSpec as FileSystemSpec
import Test.RIO.Fiber.Node.HTTPSpec as HTTPSpec
import Test.RIO.Fiber.Node.HTTP2Spec as HTTP2Spec
import Test.RIO.Fiber.Node.NetSpec as NetSpec
import Test.RIO.Fiber.Node.OSSpec as OSSpec
import Test.RIO.Fiber.Node.PathSpec as PathSpec
import Test.RIO.Fiber.Node.ProcessSpec as ProcessSpec
import Test.RIO.Fiber.Node.ReadLineSpec as ReadLineSpec
import Test.RIO.Fiber.Node.ShutdownSpec as ShutdownSpec
import Test.RIO.Fiber.Node.StreamSpec as StreamSpec
import Test.RIO.Fiber.Node.URLSpec as URLSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  BufferSpec.spec
  ChildProcessSpec.spec
  EventEmitterSpec.spec
  FileSystemSpec.spec
  HTTPSpec.spec
  HTTP2Spec.spec
  NetSpec.spec
  OSSpec.spec
  PathSpec.spec
  ProcessSpec.spec
  ReadLineSpec.spec
  ShutdownSpec.spec
  StreamSpec.spec
  URLSpec.spec
