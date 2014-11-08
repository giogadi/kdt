import Data.KdMap as KDM
import Data.Dynamic.KdMap as DKDM

import Control.Monad
import System.Exit

main :: IO ()
main = do
  success <- liftM2 (&&) KDM.runTests DKDM.runTests
  unless success exitFailure
