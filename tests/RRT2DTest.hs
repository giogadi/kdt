import Data.StateSpace
import Data.Spaces.Point2DSpace
import Data.MotionPlanningProblem (MotionPlanningProblem(..))
import Planners.RRT (solveRRTDefaultSeed, buildRRTDefaultSeed, getNumStates, writeRRT)

data Circle2D = Circle2D
  { _center :: Point2D
  , _radius :: Double
  }

pointOutsideCircle :: Circle2D -> Point2D -> Bool
pointOutsideCircle c p = stateDistanceSqrd p (_center c) > (_radius c)*(_radius c)

validMotion :: (Point2D -> Bool) -> Double -> Point2D -> Point2D -> Bool
validMotion f h p1 p2
  | h <= 0.0  = error "validMotion must have a positive step size"
  | otherwise = let n = floor $ (stateDistance p1 p2) / h :: Int
                    samplePts = scanl1 (+) (replicate n h)
                    innerValid = all f $ map (interpolate p1 p2) samplePts
                in  innerValid && (f p2)

main :: IO ()
main = let minState = Point2D 0.0 0.0
           maxState = Point2D 1.0 1.0
           circleObs = Circle2D (Point2D 0.5 0.5) 0.25
           p = MotionPlanningProblem
               { _stateSpace = makePoint2DSpace minState maxState
               , _startState = minState
               , _goalState = maxState
               , _motionValidity = validMotion (pointOutsideCircle circleObs) 0.002 }
           -- motionPlan = solveRRTDefaultSeed p 0.01 5000
           rrt = buildRRTDefaultSeed p 0.01 5000
           -- planLength = length motionPlan
       in  --putStrLn $ "Computed a motion plan with " ++ (show planLength) ++ " states."
        writeRRT rrt "/Users/luis/Desktop/rrt-test.txt"
