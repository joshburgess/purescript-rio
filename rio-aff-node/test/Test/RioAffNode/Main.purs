module Test.RioAffNode.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Node.BufferSpec as BufferSpec
import Test.RIO.Aff.Node.ChildProcessSpec as ChildProcessSpec
import Test.RIO.Aff.Node.EventEmitterSpec as EventEmitterSpec
import Test.RIO.Aff.Node.FileSystemSpec as FileSystemSpec
import Test.RIO.Aff.Node.HTTPSpec as HTTPSpec
import Test.RIO.Aff.Node.HTTP2Spec as HTTP2Spec
import Test.RIO.Aff.Node.NetSpec as NetSpec
import Test.RIO.Aff.Node.OSSpec as OSSpec
import Test.RIO.Aff.Node.PathSpec as PathSpec
import Test.RIO.Aff.Node.ProcessSpec as ProcessSpec
import Test.RIO.Aff.Node.ReadLineSpec as ReadLineSpec
import Test.RIO.Aff.Node.ShutdownSpec as ShutdownSpec
import Test.RIO.Aff.Node.StreamSpec as StreamSpec
import Test.RIO.Aff.Node.URLSpec as URLSpec

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
