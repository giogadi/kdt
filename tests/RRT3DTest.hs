import Data.StateSpace
import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (solveRRTDefaultSeed, getNumStates)

import Data.FixedList

main :: IO ()
main = let minState = 0.0 :. 0.0 :. 0.0 :. Nil
           maxState = 1.0 :. 1.0 :. 1.0 :. Nil
           p = MotionPlanningProblem
               { _stateSpace = makeRealVectorStateSpace minState maxState
               , _startState = minState
               , _goalState = maxState
               , _motionValidity = \_ _ -> True }
           motionPlan = solveRRTDefaultSeed p 0.01 1000
           planLength = Prelude.length motionPlan
       in putStrLn $ "Computed a motion plan with " ++ (show planLength) ++ " states."
