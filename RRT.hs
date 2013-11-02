module RRT
    ( MotionPlanningProblem
    , RRT
    , buildRRT
    , rrtTests
    ) where

import System.Random (randomR, RandomGen, mkStdGen)
import Control.Monad (liftM2)
import qualified Control.Monad.Random as CMR
import System.IO
import qualified Data.Sequence as Seq
import Data.List (intercalate, foldl1')
import Data.Foldable (minimumBy, foldr', toList, sum)
import Data.Function (on)
import qualified Test.QuickCheck as QC
import Control.Applicative
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

data State = State { x :: Double
                   , y :: Double } deriving (Show,Eq)

instance QC.Arbitrary State where
    arbitrary = State <$> QC.arbitrary <*> QC.arbitrary


sampleUniformState :: RandomGen g => State -> State -> CMR.Rand g State
sampleUniformState minBound maxBound =
    (liftM2 State)
           (CMR.getRandomR (x minBound, x maxBound))
           (CMR.getRandomR (y minBound, y maxBound))

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

data RoseTree a = Node a (Seq.Seq (RoseTree a))

treeSize :: RoseTree a -> Int
treeSize t = go t 0
    where go (Node x ts) i = 1 + (Data.Foldable.sum $ fmap (flip go $ 0) ts)


type TreePath = [Int]

addChildAt :: RoseTree a -> TreePath -> a -> (RoseTree a, Int)
addChildAt (Node l ts) [] s = ((Node l (ts Seq.|> (Node s Seq.empty))), Seq.length ts)
addChildAt (Node l ts) (p:ps) s = let (newChild,idx) = addChildAt (ts `Seq.index` p) ps s
                                      newTree = Node l $ Seq.update p newChild ts
                                  in  (newTree,idx)

data RRT = RRT
    { _tree :: RoseTree State
    , _stateIdx :: [(State, TreePath)] }

-- A strict implementation of minimumBy
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' min'
    where min' x y = case cmp x y of
                       GT -> x
                       _  -> y

nearestNode :: RRT -> State -> (State, TreePath)
nearestNode rrt sample = minimumBy' (compare `on` ((stateDistanceSqrd sample) . fst))
                         (_stateIdx rrt)

extendRRT :: RRT -> State -> MotionValidityFn -> Double -> RRT
extendRRT rrt sample valid maxStep =
    let (near,ps) = nearestNode rrt sample
        newState = extendTowardState near sample maxStep
    in  newState `seq`
        if valid sample newState
        then let (newTree, newIdx) = addChildAt (_tree rrt) ps newState
             in RRT newTree $ (newState,ps ++ [newIdx]) : (_stateIdx rrt)
        else rrt

buildRRT :: RandomGen g => MotionPlanningProblem -> Double -> Int -> CMR.Rand g RRT
buildRRT problem stepSize numIterations =
    let sample = sampleUniformState (_minBound problem) (_maxBound problem)
        extend state rrt = extendRRT rrt state (_motionValidity problem) stepSize
        start = _startState problem
        beginRRT = RRT (Node start Seq.empty) [(start, [])]
    in do
      stateSamples <- sequence $ replicate numIterations sample
      return $ foldr' extend beginRRT stateSamples

buildRRTDefaultSeed :: MotionPlanningProblem -> Double -> Int -> RRT
buildRRTDefaultSeed problem stepSize numIterations =
    CMR.evalRand (buildRRT problem stepSize numIterations) (mkStdGen 1)

--------------------------------------------------
-- Tests
--------------------------------------------------
prop_nonnegDist :: State -> State -> Bool
prop_nonnegDist s1 s2 = stateDistance s1 s2 >= 0.0

prop_squaredDist :: State -> State -> Bool
prop_squaredDist s1 s2 = abs ((stateDistance s1 s2)^2 - (stateDistanceSqrd s1 s2)) < 1e-5

prop_extendLimit :: State -> State -> QC.Positive Double -> Bool
prop_extendLimit s1 s2 (QC.Positive d) = let newState = extendTowardState s1 s2 d
                                         in  stateDistance s1 newState <= d + 1e-7

rrtTests = testGroup "RRT tests" [
            testProperty "Nonnegative distance" prop_nonnegDist,
            testProperty "Squared distance" prop_squaredDist,
            testProperty "Extend limit" prop_extendLimit]

-- edgesFromRRT :: Tree State -> [(State,State)]
-- edgesFromRRT tree = let treePos = fromTree tree
--                     in  collectEdges treePos (nextTree $ children treePos) []
--     where collectEdges rootPos Nothing edges = edges
--           collectEdges rootPos (Just c) edges = 
--               let childEdges = collectEdges c (nextTree $ children c) []
--                   siblingEdges = collectEdges rootPos (next c) edges
--               in (label rootPos, label c) : childEdges ++ siblingEdges
              
-- writeRRT :: RoseTree State -> String -> IO ()
-- writeRRT tree fileName = writeFile fileName $ intercalate "\n" edgeStrings
--     where edgeStrings = [stringFromEdge edge | edge <- edgesFromRRT tree]
--           stringFromEdge (s1,s2) = (stringFromState s1) ++ " " ++ (stringFromState s2)
--           stringFromState s = (show $ x s) ++ " " ++ (show $ y s)

-- main = let p = MotionPlanningProblem
--                { _startState = State 0.0 0.0
--                , _goalState = State 1.0 1.0
--                , _minBound = State 0.0 0.0
--                , _maxBound = State 1.0 1.0
--                , _motionValidity = \_ _ -> True }
--        in do
--          let rrt = buildRRTDefaultSeed p 0.01 5000
--          print $ treeSize (_tree rrt)
--          -- writeRRT (_tree rrt) "/Users/luis/Desktop/rrt.txt"
--          return ()
