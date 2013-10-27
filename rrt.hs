import System.Random (randomRIO)
import Data.Tree
import Data.Tree.Zipper
import System.IO
import Data.List (intercalate)
import Data.Foldable (minimumBy, foldr')
import Data.Function (on)

data State = State { x :: Double
                   , y :: Double } deriving (Show,Eq)

sampleUniformState :: State -> State -> IO State
sampleUniformState minBound maxBound = do
  sampleX <- randomRIO (x minBound, x maxBound)
  sampleY <- randomRIO (y minBound, y maxBound)
  return State {x = sampleX, y = sampleY}

stateDistanceSqrd :: State -> State -> Double
stateDistanceSqrd s1 s2 = let dx = (x s2) - (x s1)
                              dy = (y s2) - (y s1)
                          in dx*dx + dy*dy

stateDistance :: State -> State -> Double
stateDistance s1 s2 = sqrt $ stateDistanceSqrd s1 s2

extendTowardState :: State -> State -> Double -> State
extendTowardState nearState farState maxStepSize
    | dist <= maxStepSize = farState
    | otherwise = let scale = maxStepSize / dist
                      interpX = scale * ((x farState) - (x nearState))
                      interpY = scale * ((y farState) - (y nearState))
                  in  State {x = (x nearState) + interpX, y = (y nearState) + interpY}
    where dist = stateDistance nearState farState

type MotionValidityFn = State -> State -> Bool

data MotionPlanningProblem = MotionPlanningProblem
    { _startState :: State
    , _goalState :: State
    , _minBound :: State
    , _maxBound :: State
    , _motionValidity :: MotionValidityFn }

type TreePath = [Int]

addChildAt :: Tree a -> TreePath -> a -> (Tree a, Int)
addChildAt (Node l ts) [] s = ((Node l (ts ++ [(Node s [])])), length ts)
addChildAt (Node l ts) (p:ps) s = let (newChild,idx) = addChildAt (ts !! p) ps s
                                      newTree = Node l $ let (t1,(_:t2)) = splitAt p ts
                                                         in  t1 ++ [newChild] ++ t2
                                  in  (newTree,idx)

data RRT = RRT
    { _tree :: Tree State
    , _stateIdx :: [(State, TreePath)] }

nearestNode :: RRT -> State -> (State, TreePath)
nearestNode rrt sample = minimumBy (compare `on` ((stateDistanceSqrd sample) . fst))
                         (_stateIdx rrt)

extendRRT :: RRT -> State -> MotionValidityFn -> Double -> RRT
extendRRT rrt sample valid maxStep =
    let (near,ps) = nearestNode rrt sample
        newState = extendTowardState near sample maxStep
    in  if valid sample newState
        then let (newTree, newIdx) = addChildAt (_tree rrt) ps newState
             in RRT newTree $ (newState,ps ++ [newIdx]):(_stateIdx rrt)
        else rrt

buildRRT :: MotionPlanningProblem -> Double -> Int -> IO (RRT)
buildRRT problem stepSize numIterations =
    let sample = sampleUniformState (_minBound problem) (_maxBound problem)
        extend state rrt = extendRRT rrt state (_motionValidity problem) stepSize
        start = _startState problem
    in do
      stateSamples <- sequence $ replicate numIterations sample
      return $! foldr' extend (RRT (Node start []) [(start, [])]) stateSamples

edgesFromRRT :: Tree State -> [(State,State)]
edgesFromRRT tree = let treePos = fromTree tree
                    in  collectEdges treePos (nextTree $ children treePos) []
    where collectEdges rootPos Nothing edges = edges
          collectEdges rootPos (Just c) edges = 
              let childEdges = collectEdges c (nextTree $ children c) []
                  siblingEdges = collectEdges rootPos (next c) edges
              in (label rootPos, label c) : childEdges ++ siblingEdges
              
writeRRT :: Tree State -> String -> IO ()
writeRRT tree fileName = writeFile fileName $ intercalate "\n" edgeStrings
    where edgeStrings = [stringFromEdge edge | edge <- edgesFromRRT tree]
          stringFromEdge (s1,s2) = (stringFromState s1) ++ " " ++ (stringFromState s2)
          stringFromState s = (show $ x s) ++ " " ++ (show $ y s)

main = let p = MotionPlanningProblem
               { _startState = State 0.0 0.0
               , _goalState = State 1.0 1.0
               , _minBound = State 0.0 0.0
               , _maxBound = State 1.0 1.0
               , _motionValidity = \_ _ -> True }
       in do
         rrt <- buildRRT p 0.01 5000
         writeRRT (_tree rrt) "/Users/luis/Desktop/rrt.txt"
         return ()
