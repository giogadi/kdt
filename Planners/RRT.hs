module Planners.RRT
    ( RRT
    , buildRRT
    , buildRRTDefaultSeed
    , getNumStates
    -- , rrtTests
    ) where

import Data.StateSpace
import Data.MotionPlanningProblem

import System.Random (RandomGen, mkStdGen, StdGen)
import qualified Control.Monad.Random as CMR
import qualified Data.Sequence as Seq
import Data.List (foldl1')
import Data.Foldable (foldr', sum)
import Data.Function (on)
-- import qualified Test.QuickCheck as QC
-- import Test.Framework (testGroup)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

data RoseTree a = Node a (Seq.Seq (RoseTree a))

treeSize :: RoseTree a -> Int
treeSize (Node _ ts) = 1 + (Data.Foldable.sum $ fmap treeSize ts)

type TreePath = [Int]

addChildAt :: RoseTree a -> TreePath -> a -> (RoseTree a, Int)
addChildAt (Node l ts) [] s = ((Node l (ts Seq.|> (Node s Seq.empty))), Seq.length ts)
addChildAt (Node l ts) (p:ps) s = let (newChild,idx) = addChildAt (ts `Seq.index` p) ps s
                                      newTree = Node l $ Seq.update p newChild ts
                                  in  (newTree,idx)

data RRT s g = RRT
    { _problem  :: MotionPlanningProblem s g
    , _stepSize :: Double
    , _tree     :: RoseTree s
    , _stateIdx :: [(s, TreePath)] }

getSpace :: RRT s g -> StateSpace s g
getSpace = _stateSpace . _problem

getNonMetricDist :: RRT s g -> (s -> s -> Double)
getNonMetricDist rrt = _fastNonMetricDistance $ getSpace rrt

getValidityFn :: RRT s g -> MotionValidityFn s
getValidityFn rrt = _motionValidity $ _problem rrt

getInterp :: RRT s g -> (s -> s -> Double -> s)
getInterp rrt = _interpolate $ getSpace rrt

getNumStates :: RRT s g -> Int
getNumStates = treeSize . _tree

-- A strict implementation of minimumBy
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' min'
    where min' x y = case cmp x y of
                       GT -> x
                       _  -> y

nearestNode :: RRT s g -> s -> (s, TreePath)
nearestNode rrt sample = let compareFn = compare `on` ((getNonMetricDist rrt $ sample) . fst)
                         in  minimumBy' compareFn (_stateIdx rrt)

extendRRT :: s -> RRT s g -> RRT s g
extendRRT sample rrt =
    let (near,ps) = nearestNode rrt sample
        newState = (getInterp rrt) near sample $ _stepSize rrt
    in  newState `seq`
        if (getValidityFn rrt) sample newState
        then let (newTree, newIdx) = addChildAt (_tree rrt) ps newState
             in RRT 
                    (_problem rrt) 
                    (_stepSize rrt) 
                    newTree $ 
                    (newState,ps ++ [newIdx]) : (_stateIdx rrt)
        else rrt

buildRRT :: RandomGen g => MotionPlanningProblem s g -> Double -> Int -> CMR.Rand g (RRT s g)
buildRRT problem stepSize numIterations =
    let sample = _sampleUniform $ _stateSpace problem
        start = _startState problem
        beginRRT = RRT problem stepSize (Node start Seq.empty) [(start, [])]
    in do
      stateSamples <- sequence $ replicate numIterations sample
      return $ foldr' extendRRT beginRRT stateSamples

buildRRTDefaultSeed :: MotionPlanningProblem s StdGen -> Double -> Int -> RRT s StdGen
buildRRTDefaultSeed problem stepSize numIterations =
    CMR.evalRand (buildRRT problem stepSize numIterations) (mkStdGen 1)

-- --------------------------------------------------
-- -- Tests
-- --------------------------------------------------
-- prop_nonnegDist :: State -> State -> Bool
-- prop_nonnegDist s1 s2 = stateDistance s1 s2 >= 0.0

-- prop_squaredDist :: State -> State -> Bool
-- prop_squaredDist s1 s2 = abs ((stateDistance s1 s2)^2 - (stateDistanceSqrd s1 s2)) < 1e-5

-- prop_extendLimit :: State -> State -> QC.Positive Double -> Bool
-- prop_extendLimit s1 s2 (QC.Positive d) = let newState = extendTowardState s1 s2 d
--                                          in  stateDistance s1 newState <= d + 1e-7

-- rrtTests = testGroup "RRT tests" [
--             testProperty "Nonnegative distance" prop_nonnegDist,
--             testProperty "Squared distance" prop_squaredDist,
--             testProperty "Extend limit" prop_extendLimit]

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
