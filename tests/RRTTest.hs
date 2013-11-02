import Data.StateSpace
-- import Data.Spaces.RealVectorStateSpace (makeRealVectorStateSpace)
import Data.Spaces.Point2DSpace
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (buildRRTDefaultSeed, getNumStates)

-- main = let minBound = 0.0 :. 0.0 :. Nil
--            maxBound = 1.0 :. 1.0 :. Nil
--            p = MotionPlanningProblem
--                { _stateSpace = makeRealVectorStateSpace minBound maxBound
--                , _startState = minBound
--                , _goalState = maxBound
--                , _motionValidity = \s1 s2 -> True }
--            rrt = buildRRTDefaultSeed p 0.01 1000
--        in  print $ getNumStates rrt

main = let minBound = Point2D 0.0 0.0
           maxBound = Point2D 1.0 1.0
           p = MotionPlanningProblem
               { _stateSpace = makePoint2DSpace minBound maxBound
               , _startState = minBound
               , _goalState = maxBound
               , _motionValidity = \s1 s2 -> True }
           rrt = buildRRTDefaultSeed p 0.01 5000
       in  print $ getNumStates rrt
