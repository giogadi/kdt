import Data.StateSpace
-- import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.Spaces.Point2DSpace
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (buildRRTDefaultSeed, getNumStates)

-- import Data.FixedList

-- main :: IO ()
-- main = let minState = 0.0 :. 0.0 :. Nil
--            maxState = 1.0 :. 1.0 :. Nil
--            p = MotionPlanningProblem
--                { _stateSpace = makeRealVectorStateSpace minState maxState
--                , _startState = minState
--                , _goalState = maxState
--                , _motionValidity = \_ _ -> True }
--            rrt = buildRRTDefaultSeed p 0.01 1000
--        in  print $ getNumStates rrt

main :: IO ()
main = let minState = Point2D 0.0 0.0
           maxState = Point2D 1.0 1.0
           p = MotionPlanningProblem
               { _stateSpace = makePoint2DSpace minState maxState
               , _startState = minState
               , _goalState = maxState
               , _motionValidity = \_ _ -> True }
           rrt = buildRRTDefaultSeed p 0.01 5000
       in  print $ getNumStates rrt
