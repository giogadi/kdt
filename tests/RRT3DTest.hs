import Data.StateSpace
import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.MotionPlanningProblem
import Planners.RRT (solveRRTDefaultSeed, getNumStates)

import Data.FixedList

main :: IO ()
main = let minState = 0.0 :. 0.0 :. 0.0 :. Nil
           maxState = 1.0 :. 1.0 :. 1.0 :. Nil
           ss = makeRealVectorStateSpace minState maxState
           p = MotionPlanningProblem
               { _stateSpace = ss
               , _startState = minState
               , _goalSatisfied = goalStateSatisfied ss 0.01 maxState
               , _motionValidity = \_ _ -> True }
           motionPlan = solveRRTDefaultSeed p 0.01 1000
           planLength = Prelude.length motionPlan
       in putStrLn $ "Computed a motion plan with " ++ (show planLength) ++ " states."
