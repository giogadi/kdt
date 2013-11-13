import Data.StateSpace
import Data.Spaces.Point2DSpace
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (solveRRTDefaultSeed, getNumStates, writeRRT)

main :: IO ()
main = let minState = Point2D 0.0 0.0
           maxState = Point2D 1.0 1.0
           p = MotionPlanningProblem
               { _stateSpace = makePoint2DSpace minState maxState
               , _startState = minState
               , _goalState = maxState
               , _motionValidity = \_ _ -> True }
           motionPlan = solveRRTDefaultSeed p 0.01 5000
           planLength = length motionPlan
       in  putStrLn $ "Computed a motion plan with " ++ (show planLength) ++ " states."
