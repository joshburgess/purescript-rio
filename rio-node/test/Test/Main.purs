module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Node.FileSystemSpec as FileSystemSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  FileSystemSpec.spec
