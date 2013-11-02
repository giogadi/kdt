import Data.StateSpace
import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (buildRRTDefaultSeed, getNumStates)

import Data.FixedList

main = let minBound = 0.0 :. 0.0 :. Nil
           maxBound = 1.0 :. 1.0 :. Nil
           p = MotionPlanningProblem
               { _stateSpace = makeRealVectorStateSpace minBound maxBound
               , _startState = minBound
               , _goalState = maxBound
               , _motionValidity = \s1 s2 -> True }
           rrt = buildRRTDefaultSeed p 0.01 100
       in  print $ getNumStates rrt
  
