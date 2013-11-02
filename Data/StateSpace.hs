module Data.StateSpace
    ( StateSpace(..) ) where

import qualified Control.Monad.Random as CMR

data StateSpace s g = StateSpace
    { _stateDistance :: s -> s -> Double
    , _fastNonMetricDistance :: s -> s -> Double
    , _interpolate :: s -> s -> Double -> s
    , _sampleUniform :: CMR.Rand g s
    }
