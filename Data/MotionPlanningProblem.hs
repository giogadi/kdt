module Data.MotionPlanningProblem
       ( MotionValidityFn
       , MotionPlanningProblem(..)
       , discreteMotionValid
       , goalStateSatisfied ) where

import Data.StateSpace
import Debug.Trace (trace)

type MotionValidityFn s = s -> s -> Bool

data MotionPlanningProblem s g = MotionPlanningProblem
    { _stateSpace     :: StateSpace s g
    , _startState     :: s
    , _goalSatisfied  :: s -> Bool
    , _motionValidity :: MotionValidityFn s}

discreteMotionValid :: StateSpace s g -> (s -> Bool) -> Double -> s -> s -> Bool
discreteMotionValid ss f h s1 s2
  | h <= 0.0  = error "Data.MotionPlanningProblem.discreteMotionValid must have a positive step size"
  | otherwise = let n = floor $ ((_stateDistance ss) s1 s2) / h :: Int
                    samplePts = scanl1 (+) (replicate n h)
                    innerValid = all f $ map ((_interpolate ss) s1 s2) samplePts
                in  innerValid && (f s2)

goalStateSatisfied :: StateSpace s g -> Double -> s -> (s -> Bool)
goalStateSatisfied ss tol goalState s =
  (_fastNonMetricDistance ss) s goalState <= tol*tol
