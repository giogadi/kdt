module Data.MotionPlanningProblem
       ( MotionValidityFn
       , MotionPlanningProblem(..) ) where

import Data.StateSpace

type MotionValidityFn s = s -> s -> Bool

data MotionPlanningProblem s g = MotionPlanningProblem
    { _stateSpace :: StateSpace s g
    , _startState :: s
    , _goalState  :: s
    , _motionValidity :: MotionValidityFn s}
