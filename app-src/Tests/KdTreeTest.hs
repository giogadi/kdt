import Data.Trees.KdMap as KDM
import Data.Trees.DynamicKdTree as DKDT

import Control.Monad
import System.Exit

main :: IO ()
main = do
  success <- liftM2 (&&) KDM.runTests DKDT.runTests
  when (not success) exitFailure
