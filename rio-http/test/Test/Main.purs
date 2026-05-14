module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.HTTPurple.AuthSpec as AuthSpec
import Test.RIO.HTTPurple.MiddlewareSpec as MiddlewareSpec
import Test.RIO.HTTPurple.RequestSpec as RequestSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  AuthSpec.spec
  RequestSpec.spec
  MiddlewareSpec.spec
