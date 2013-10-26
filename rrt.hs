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

stateDistance :: State -> State -> Double
stateDistance s1 s2 = let dx = (x s2) - (x s1)
                          dy = (y s2) - (y s1)
                      in  sqrt $ dx*dx + dy*dy

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

-- implemented with tail recursion.
childrenAsList :: TreePos Full a -> [TreePos Full a ] -> [TreePos Full a]
childrenAsList = go . children where
    go z zs = case nextTree z of
                Nothing -> zs
                Just z -> go (nextSpace z) $! (z:zs)

nearestInTree :: TreePos Full State -> State -> TreePos Full State
nearestInTree z q = go z where
    go z = minimumBy (compare `on` (stateDistance q) . label)
           (z : map go (childrenAsList z []))

extendRRT :: TreePos Full State -> State -> MotionValidityFn -> Double -> TreePos Full State
extendRRT z sample valid maxStep =
    let near = nearestInTree z sample
        newNode = extendTowardState (label near) sample maxStep
    in  if valid (label near) newNode
        then root $ insert (Node newNode []) (children near)
        else z

buildRRT :: MotionPlanningProblem -> Double -> Int -> IO (Tree State)
buildRRT problem stepSize numIterations = 
    let sample = sampleUniformState (_minBound problem) (_maxBound problem)
        extend state z = extendRRT z state (_motionValidity problem) stepSize
    in do
      stateSamples <- sequence $ replicate numIterations sample
      return $! toTree $ foldr' extend (fromTree $ (Node (_startState problem) [])) stateSamples

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
         tree <- buildRRT p 0.01 5000
         -- writeRRT tree "/Users/luis/Desktop/rrt.txt"
         return ()
