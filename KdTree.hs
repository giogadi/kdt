{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.KdTree
       ( EuclideanSpace (..)
       , KdTree
       , buildKdTree
       , toList
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , mk2DEuclideanSpace
       , Point2d (..)
       , runTests
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import Data.Function
import qualified Data.List as L
import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Set as S
import qualified Data.Vector as V
import Test.QuickCheck

data EuclideanSpace p = EuclideanSpace
                        { _dimension :: Int
                        , _coord     :: Int -> p -> Double
                        , _dist2     :: p -> p -> Double
                        } deriving Generic
instance NFData p => NFData (EuclideanSpace p) where rnf = genericRnf

incrementAxis :: EuclideanSpace s -> Int -> Int
incrementAxis s axis = (axis + 1) `mod` _dimension s

data TreeNode p d = TreeNode { treeLeft :: Maybe (TreeNode p d)
                             , treePoint :: (p, d)
                             , treeRight :: Maybe (TreeNode p d)
                             }
                             deriving Generic
instance (NFData p, NFData d) => NFData (TreeNode p d) where rnf = genericRnf

data KdTree p d = KdTree (EuclideanSpace p) (TreeNode p d) deriving Generic
instance (NFData p, NFData d) => NFData (KdTree p d) where rnf = genericRnf

buildTreeInternal :: EuclideanSpace p -> [V.Vector (p, d)] -> Int -> TreeNode p d
buildTreeInternal s sortedByAxis axis
  | n == 1 = TreeNode Nothing (V.head $ head sortedByAxis) Nothing
  | otherwise =
    let medianIx = n `div` 2 :: Int
        median   = (sortedByAxis !! axis) V.! medianIx
        splitVal = _coord s axis $ fst median
        partitionVec vec vAxis =
          if vAxis == axis
          then let (left, medAndRight) = V.splitAt medianIx vec
               in  (left, V.tail medAndRight)
          else let (left, medAndRight) = V.partition ((< splitVal) . _coord s axis . fst) vec
               in  case V.findIndex ((== splitVal) . _coord s axis . fst) medAndRight of
                     Just ix -> let (l, r) = V.splitAt ix medAndRight
                                in  (left, l V.++ V.tail r)
                     Nothing -> error "we done fucked up yo"
        (leftPoints, rightPoints) = unzip $ zipWith partitionVec sortedByAxis [0..]
    in  TreeNode
        { treeLeft = maybeBuildTree leftPoints
        , treePoint = median
        , treeRight = maybeBuildTree rightPoints
        }
  where n = V.length $ head sortedByAxis
        maybeBuildTree ps
          | V.null $ head ps = Nothing
          | otherwise = Just $ buildTreeInternal s ps $ incrementAxis s axis

buildKdTree :: EuclideanSpace p -> [(p, d)] -> KdTree p d
buildKdTree _ [] = error "Who wants an empty KdTree anyway?"
buildKdTree s ps = let sortByAxis points a = L.sortBy (compare `on` (_coord s a . fst)) points
                       sortedByAxis = map (V.fromList . sortByAxis ps) [0 .. (_dimension s - 1)]
                   in  KdTree s $ buildTreeInternal s sortedByAxis 0

-- TODO: just make KdTree foldable I guess
toListInternal :: TreeNode p d -> [(p, d)]
toListInternal t = go (Just t) []
  where go Nothing = id
        go (Just (TreeNode l p r)) = go l . (p :) . go r

toList :: KdTree p d -> [(p, d)]
toList (KdTree _ t) = toListInternal t

isTreeValid :: EuclideanSpace p -> Int -> TreeNode p d -> Bool
isTreeValid s axis (TreeNode l (p, _) r) =
  let xAxisVal = _coord s axis p
      nodeKey (TreeNode _ (p', _) _) = p'
      nextAxis = incrementAxis s axis
      leftChildValid = maybe True ((<= xAxisVal) . _coord s axis . nodeKey) l
      rightChildValid = maybe True ((> xAxisVal) . _coord s axis . nodeKey) r
      leftSubtreeValid = maybe True (isTreeValid s nextAxis) l
      rightSubtreeValid = maybe True (isTreeValid s nextAxis) r
  in  leftChildValid && rightChildValid && leftSubtreeValid && rightSubtreeValid

nearestNeighbor :: KdTree p d -> p -> (p, d)
nearestNeighbor (KdTree s t@(TreeNode _ root _)) query =
  -- This is an ugly way to kickstart the function but it's faster
  -- than using a Maybe.
  fst $ go 0 (root, 1 / 0 :: Double) t
  where
    go axis bestSoFar (TreeNode maybeLeft (curr_p, curr_d) maybeRight) =
      let better (x1, dist1) (x2, dist2) = if dist1 < dist2
                                           then (x1, dist1)
                                           else (x2, dist2)
          queryAxisValue = _coord s axis query
          currAxisValue  = _coord s axis curr_p
          currDist       = _dist2 s query curr_p
          nextAxis       = incrementAxis s axis
          bestAfterCurr = better ((curr_p, curr_d), currDist) bestSoFar
          nearestInTree maybeOnsideTree maybeOffsideTree =
            let bestAfterOnside =
                  maybe bestAfterCurr (go nextAxis $ bestAfterCurr) maybeOnsideTree
                checkOffsideTree =
                  (queryAxisValue - currAxisValue)^(2 :: Int) < snd bestAfterOnside
            in  if checkOffsideTree
                then maybe bestAfterOnside (go nextAxis $ bestAfterOnside) maybeOffsideTree
                else bestAfterOnside
      in  if queryAxisValue <= currAxisValue
          then nearestInTree maybeLeft maybeRight
          else nearestInTree maybeRight maybeLeft

-- TODO why isn't this faster than the concat-based version?
-- nearNeighbors :: KdTree p d -> Double -> p -> [(p, d)]
-- nearNeighbors (KdTree s t) radius query = go 0 t []
--   where go axis (TreeNode maybeLeft (p, d) maybeRight) =
--           let xAxisVal     = _coord s axis p
--               queryAxisVal = _coord s axis query
--               nextAxis     = incrementAxis s axis
--               nears = maybe id (go nextAxis)
--               onTheLeft = queryAxisVal <= xAxisVal
--               onsideNear = if onTheLeft
--                            then nears maybeLeft
--                            else nears maybeRight
--               offsideNear = if abs (queryAxisVal - xAxisVal) < radius
--                             then if onTheLeft
--                                  then nears maybeRight
--                                  else nears maybeLeft
--                             else id
--               currentNear = if _dist2 s p query <= radius * radius
--                             then ((p, d) :)
--                             else id
--           in  onsideNear . currentNear . offsideNear

nearNeighbors :: KdTree p d -> Double -> p -> [(p, d)]
nearNeighbors (KdTree s t) radius query = go 0 t
  where go axis (TreeNode maybeLeft (p, d) maybeRight) =
          let xAxisVal     = _coord s axis p
              queryAxisVal = _coord s axis query
              nextAxis     = incrementAxis s axis
              nears = maybe [] (go nextAxis)
              onTheLeft = queryAxisVal <= xAxisVal
              onsideNear = if onTheLeft
                           then nears maybeLeft
                           else nears maybeRight
              offsideNear = if abs (queryAxisVal - xAxisVal) < radius
                            then if onTheLeft
                                 then nears maybeRight
                                 else nears maybeLeft
                            else []
              currentNear = if _dist2 s p query <= radius * radius
                            then [(p, d)]
                            else []
          in  onsideNear ++ currentNear ++ offsideNear

kNearestNeighbors :: KdTree p d -> Int -> p -> [(p, d)]
kNearestNeighbors (KdTree s t) k query = reverse $ map snd $ Q.toList $ go 0 Q.empty t
  where -- go :: Int -> Q.MaxPQueue Double (p, d) -> TreeNode p d -> KQueue p d
        go axis q (TreeNode maybeLeft (p, d) maybeRight) =
          let queryAxisValue = _coord s axis query
              xAxisValue = _coord s axis p
              insertBounded queue dist x
                | Q.size queue < k = Q.insert dist x queue
                | otherwise = if dist < fst (Q.findMax queue)
                              then Q.deleteMax $ Q.insert dist x queue
                              else queue
              q' = insertBounded q (_dist2 s p query) (p, d)
              kNearest queue maybeOnsideTree maybeOffsideTree =
                let queue' = maybe queue (go nextAxis queue) maybeOnsideTree
                    nextAxis = incrementAxis s axis
                    checkOffsideTree =
                      Q.size queue' < k ||
                      (queryAxisValue - xAxisValue)^(2 :: Int) < fst (Q.findMax queue')
                in  if checkOffsideTree
                    then maybe queue' (go nextAxis queue') maybeOffsideTree
                    else queue'
          in  if queryAxisValue <= xAxisValue
              then kNearest q' maybeLeft maybeRight
              else kNearest q' maybeRight maybeLeft

data Point2d = Point2d Double Double deriving (Show, Eq, Ord, Generic)
instance NFData Point2d where rnf = genericRnf

mk2DEuclideanSpace :: EuclideanSpace Point2d
mk2DEuclideanSpace = EuclideanSpace
                     { _dimension = 2
                     , _coord = coord
                     , _dist2 = dist
                     }
 where coord 0 (Point2d x _) = x
       coord 1 (Point2d _ y) = y
       coord _ _ = error "Tried to access invalid coordinate of Point2d!"

       dist (Point2d x1 y1) (Point2d x2 y2) = let dx = x2 - x1
                                                  dy = y2 - y1
                                              in dx*dx + dy*dy

instance Arbitrary Point2d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Point2d x y)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps $ [0 ..]

checkValidTree :: EuclideanSpace p -> [p] -> Bool
checkValidTree s ps =
  let (KdTree _ t) = buildKdTree s $ testElements ps
  in  isTreeValid s 0 t

prop_validTree :: Property
prop_validTree = forAll (listOf1 arbitrary) $ checkValidTree mk2DEuclideanSpace

nearestNeighborLinear :: EuclideanSpace p -> [(p, d)] -> p -> (p, d)
nearestNeighborLinear s xs query =
  L.minimumBy (compare `on` (_dist2 s query . fst)) xs

checkNearestEqualToLinear :: Eq p => EuclideanSpace p -> ([p], p) -> Bool
checkNearestEqualToLinear s (ps, query) =
  let kdt = buildKdTree s $ testElements ps
  in  nearestNeighbor kdt query == nearestNeighborLinear s (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear mk2DEuclideanSpace (xs, query)

nearNeighborsLinear :: EuclideanSpace p -> [(p, d)] -> p -> Double -> [(p, d)]
nearNeighborsLinear s xs query radius =
  filter ((<= radius * radius) . _dist2 s query . fst) xs

checkNearEqualToLinear :: Ord p => EuclideanSpace p -> Double -> ([p], p) -> Bool
checkNearEqualToLinear s radius (ps, query) =
  let kdt = buildKdTree s $ testElements ps
      kdtNear = nearNeighbors kdt radius query
      linearNear = nearNeighborsLinear s (testElements ps) query radius
  in  S.fromList kdtNear == S.fromList linearNear

prop_nearEqualToLinear :: Point2d -> Property
prop_nearEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkNearEqualToLinear mk2DEuclideanSpace radius (xs, query)

kNearestNeighborsLinear :: EuclideanSpace p -> [(p, d)] -> p -> Int -> [(p, d)]
kNearestNeighborsLinear s xs query k =
  take k $ L.sortBy (compare `on` (_dist2 s query . fst)) xs

checkKNearestEqualToLinear :: Ord p => EuclideanSpace p -> Int -> ([p], p) -> Bool
checkKNearestEqualToLinear s k (xs, query) =
  let kdt = buildKdTree s $ testElements xs
      kdtKNear = kNearestNeighbors kdt k query
      linearKNear = kNearestNeighborsLinear s (testElements xs) query k
  in  S.fromList kdtKNear == S.fromList linearKNear

prop_kNearestEqualToLinear :: Point2d -> Property
prop_kNearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToLinear mk2DEuclideanSpace k (xs, query)

checkKNearestSorted :: Eq p => EuclideanSpace p -> ([p], p) -> Bool
checkKNearestSorted _ ([], _) = True
checkKNearestSorted s (ps, query) =
  let kdt = buildKdTree s $ testElements ps
      kNearestDists = map (_dist2 s query . fst) $ kNearestNeighbors kdt (length ps) query
  in  kNearestDists == L.sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted mk2DEuclideanSpace (xs, query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll
