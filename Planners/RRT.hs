module Planners.RRT
    ( RRT
    , buildRRT
    , buildRRTDefaultSeed
    , getNumStates
    , writeRRT
    -- , rrtTests
    ) where

import Data.StateSpace
import Data.MotionPlanningProblem

import Debug.Trace (trace)
import Data.Maybe (isJust, fromJust)
import System.Random (RandomGen, mkStdGen, StdGen)
import qualified Control.Monad.Random as CMR
import qualified Data.Sequence as Seq
import Data.List (foldl1', intercalate)
import Data.Foldable (foldr', sum, toList)
import Data.Function (on)
-- import qualified Test.QuickCheck as QC
-- import Test.Framework (testGroup)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

data RoseTree a = Node a (Seq.Seq (RoseTree a))

rootLabel :: RoseTree a -> a
rootLabel (Node s _) = s

treeSize :: RoseTree a -> Int
treeSize (Node _ ts) = 1 + (Data.Foldable.sum $ fmap treeSize ts)

treeEdges :: RoseTree a -> [(a,a)]
treeEdges t = go t
    where go (Node s ts) = let listT = toList ts
                           in  (zip (repeat s) $ rootLabel `fmap` listT) ++ (concat (go `fmap` listT))

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

getDist :: RRT s g -> (s -> s -> Double)
getDist rrt = _stateDistance $ getSpace rrt

getValidityFn :: RRT s g -> MotionValidityFn s
getValidityFn rrt = _motionValidity $ _problem rrt

getInterp :: RRT s g -> (s -> s -> Double -> s)
getInterp rrt = _interpolate $ getSpace rrt

getNumStates :: RRT s g -> Int
getNumStates = treeSize . _tree

writeRRT :: Show s => RRT s g -> String -> IO ()
writeRRT rrt fileName = writeFile fileName $ intercalate "\n" edgeStrings
    where edgeStrings = map stringFromEdge $ treeEdges (_tree rrt)
          stringFromEdge (s1,s2) = (show s1) ++ " " ++ (show s2)

-- A strict implementation of minimumBy
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' min'
    where min' x y = case cmp x y of
                       GT -> y
                       _  -> x

nearestNode :: RRT s g -> s -> (s, TreePath)
nearestNode rrt sample = let compareFn = compare `on` ((getNonMetricDist rrt $ sample) . fst)
                         in  minimumBy' compareFn (_stateIdx rrt)

extendRRT :: RRT s g -> s -> (RRT s g, Maybe s)
extendRRT rrt sample =
    let (near,ps) = nearestNode rrt sample
        newState = let d = (getDist rrt) near sample
                   in  if d <= (_stepSize rrt)
                       then (getInterp rrt) near sample 1.0
                       else (getInterp rrt) near sample $ (_stepSize rrt) / d
    in  newState `seq`
        if (getValidityFn rrt) sample newState
          then let (newTree, newIdx) = addChildAt (_tree rrt) ps newState
                   newRRT = RRT
                            (_problem rrt)
                            (_stepSize rrt)
                            newTree $ (newState,ps ++ [newIdx]) : (_stateIdx rrt)
               in  (newRRT, Just newState)
          else (rrt, Nothing)

buildRRT :: RandomGen g => MotionPlanningProblem s g -> Double -> Int -> CMR.Rand g (RRT s g)
buildRRT problem stepSize numIterations =
    let start = _startState problem
        beginRRT = RRT problem stepSize (Node start Seq.empty) [(start, [])]
    in  go beginRRT Nothing 0
    where -- go :: RandomGen g => (RRT s g) -> Maybe s -> Int -> CMR.Rand g (RRT s g)
          go rrt lastState iteration
              | iteration >= numIterations = return rrt
              | (isJust lastState) && inGoal (fromJust lastState) = return rrt
              | otherwise = do
                              (newRRT, newState) <- (extendRRT rrt) `fmap` sample
                              go newRRT newState (iteration + 1)
              where inGoal state = ((getNonMetricDist rrt) state goal) <= 0.01
                    goal = _goalState problem
                    sample = _sampleUniform $ _stateSpace problem

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
