import Data.StateSpace
import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.MotionPlanningProblem
import Planners.RRT

import Data.FixedList

main :: IO ()
main = let minState = 0.0 :. 0.0 :. 0.0 :. Nil
           maxState = 1.0 :. 1.0 :. 1.0 :. Nil
           ss = makeRealVectorStateSpace minState maxState
           p = MotionPlanningProblem
               { _stateSpace = ss
               , _startState = minState
               , _goalSatisfied = goalStateSatisfied ss 0.2 maxState
               , _motionValidity = \_ _ -> True }
           rrt = buildRRTDefaultSeed p 0.1 1000
           motionPlan = getPathToGoal rrt
       in do
         putStrLn $ "Computed a motion plan with " ++ (show $ Prelude.length motionPlan) ++ " states."
         putStrLn $ "Num states in tree: " ++ (show $ getNumStates rrt)
         putStrLn $ "Plan:"
         mapM_ (putStrLn . show) motionPlan
