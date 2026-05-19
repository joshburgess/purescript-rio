-- | Test-side helpers: bridge the rio-fiber callback runner into
-- | `Aff` so spec bodies can `await` an outcome with normal `do`.
module Test.RIO.Fiber.Helpers
  ( runAff
  ) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome, RIO, runRIOCallback)

runAff :: forall r e a. RIO r e a -> Record r -> Aff (Outcome e a)
runAff rio env = makeAff \cb -> do
  cancel <- runRIOCallback rio env (cb <<< Right)
  pure (Canceler \_ -> liftEffect cancel)
