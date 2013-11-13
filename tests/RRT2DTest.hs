import Data.StateSpace
import Data.Spaces.Point2DSpace
import Data.MotionPlanningProblem
import Planners.RRT (solveRRTDefaultSeed, buildRRTDefaultSeed, getNumStates, writeRRT)

data Circle2D = Circle2D
  { _center :: Point2D
  , _radius :: Double
  }

pointOutsideCircle :: Circle2D -> Point2D -> Bool
pointOutsideCircle c p = stateDistanceSqrd p (_center c) > (_radius c)*(_radius c)

main :: IO ()
main = let minState = Point2D 0.0 0.0
           maxState = Point2D 1.0 1.0
           circleObs = Circle2D (Point2D 0.5 0.5) 0.25
           ss = makePoint2DSpace minState maxState
           p = MotionPlanningProblem
               { _stateSpace = ss
               , _startState = minState
               , _goalSatisfied = goalStateSatisfied ss 0.01 maxState
               , _motionValidity = discreteMotionValid ss (pointOutsideCircle circleObs) 0.002 }
           -- motionPlan = solveRRTDefaultSeed p 0.01 5000
           rrt = buildRRTDefaultSeed p 0.01 5000
           -- planLength = length motionPlan
       in  --putStrLn $ "Computed a motion plan with " ++ (show planLength) ++ " states."
        writeRRT rrt "rrt-test.txt"
