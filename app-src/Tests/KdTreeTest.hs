import Data.Trees.KdMap as KDM
import Data.Trees.DynamicKdMap as DKDM

import Control.Monad
import System.Exit

main :: IO ()
main = do
  success <- liftM2 (&&) KDM.runTests DKDM.runTests
  unless success exitFailure
