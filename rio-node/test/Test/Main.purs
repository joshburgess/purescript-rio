module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Node.BufferSpec as BufferSpec
import Test.RIO.Node.ChildProcessSpec as ChildProcessSpec
import Test.RIO.Node.EventEmitterSpec as EventEmitterSpec
import Test.RIO.Node.FileSystemSpec as FileSystemSpec
import Test.RIO.Node.HTTPSpec as HTTPSpec
import Test.RIO.Node.HTTP2Spec as HTTP2Spec
import Test.RIO.Node.NetSpec as NetSpec
import Test.RIO.Node.OSSpec as OSSpec
import Test.RIO.Node.PathSpec as PathSpec
import Test.RIO.Node.ProcessSpec as ProcessSpec
import Test.RIO.Node.ReadLineSpec as ReadLineSpec
import Test.RIO.Node.ShutdownSpec as ShutdownSpec
import Test.RIO.Node.StreamSpec as StreamSpec
import Test.RIO.Node.URLSpec as URLSpec

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
