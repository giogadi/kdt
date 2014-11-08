import Data.KdMap.Static as KDM
import Data.KdMap.Dynamic as DKDM

import Control.Monad
import System.Exit

main :: IO ()
main = do
  success <- liftM2 (&&) KDM.runTests DKDM.runTests
  unless success exitFailure
