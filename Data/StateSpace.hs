module Data.StateSpace
    ( StateSpace(..) ) where

import qualified Control.Monad.Random as CMR

data StateSpace s g = StateSpace
    { stateDistance :: s -> s -> Double
    , fastNonMetricDistance :: s -> s -> Double
    , interpolate :: s -> s -> Double -> s
    , sampleUniform :: CMR.Rand g s
    }
