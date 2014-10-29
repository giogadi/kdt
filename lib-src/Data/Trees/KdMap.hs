{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.KdMap
       ( KdSpace (..)
       , KdMap
       , buildKdMap
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , size
       , assocs
       , keys
       , values
       , mk2DEuclideanSpace
       , Point2d (..)
       , runTests
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import Data.Foldable
import Data.Function
import qualified Data.List as L
import Data.Ord
import qualified Data.PQueue.Prio.Max as Q
import Test.QuickCheck

data KdSpace k = KdSpace
                 { _numDimensions :: Int
                 , _coord         :: Int -> k -> Double
                 , _distSqr       :: k -> k -> Double
                 } deriving Generic
instance NFData k => NFData (KdSpace k) where rnf = genericRnf

incrementAxis :: KdSpace k -> Int -> Int
incrementAxis s axis = (axis + 1) `mod` _numDimensions s

data TreeNode k v = TreeNode { treeLeft :: Maybe (TreeNode k v)
                             , treePoint :: (k, v)
                             , treeRight :: Maybe (TreeNode k v)
                             }
                             deriving Generic
instance (NFData k, NFData v) => NFData (TreeNode k v) where rnf = genericRnf

mapTreeNode :: (v1 -> v2) -> TreeNode k v1 -> TreeNode k v2
mapTreeNode f (TreeNode maybeLeft (k, v) maybeRight) =
  TreeNode (fmap (mapTreeNode f) maybeLeft) (k, f v) (fmap (mapTreeNode f) maybeRight)

data KdMap k v = KdMap (KdSpace k) (TreeNode k v) Int deriving Generic
instance (NFData k, NFData v) => NFData (KdMap k v) where rnf = genericRnf

instance Functor (KdMap k) where
  fmap f (KdMap s t n) = KdMap s (mapTreeNode f t) n

instance Foldable (KdMap k) where
  foldr f z (KdMap _ t _) = go f z t
    where go f' z' (TreeNode Nothing (_, v) Nothing) = f' v z'
          go f' z' (TreeNode (Just l) (_, v) Nothing) = go f' (f' v z') l
          go f' z' (TreeNode Nothing (_, v) (Just r)) = f' v (go f' z' r)
          go f' z' (TreeNode (Just l) (_, v) (Just r)) = go f' (f' v (go f' z' r)) l

quickselect :: (a -> a -> Ordering) -> Int -> [a] -> a
quickselect cmp = go
  where go _ [] = error "quickselect must be called on a non-empty list."
        go k (x:xs) | k < l = go k ys
                    | k > l = go (k - l - 1) zs
                    | otherwise = x
          where (ys, zs) = L.partition ((== LT) . (`cmp` x)) xs
                l = length ys

buildKdMap :: KdSpace k -> [(k, v)] -> KdMap k v
buildKdMap _ [] = error "KdMap must be built with a non-empty list."
buildKdMap s points = KdMap s (buildTreeInternal points 0) $ length points
  where buildTreeInternal [p] _ = TreeNode Nothing p Nothing
        buildTreeInternal ps axis =
          let n = length ps
              axisVals = map (_coord s axis . fst) ps
              axisValsPoints = zip axisVals ps
              (medianAxisVal, medianPt) =
                quickselect (comparing fst) (n `div` 2) axisValsPoints
              f (v, p) (lt, gt) | v < medianAxisVal = (p : lt, gt)
                                | v > medianAxisVal = (lt, p : gt)
                                | otherwise = (lt, gt)
              (leftPoints, rightPoints) = L.foldr f ([], []) axisValsPoints
              maybeBuildTree [] = Nothing
              maybeBuildTree ps' = Just $ buildTreeInternal ps' $ incrementAxis s axis
          in  TreeNode
              { treeLeft = maybeBuildTree leftPoints
              , treePoint = medianPt
              , treeRight = maybeBuildTree rightPoints
              }

assocs :: KdMap k v -> [(k, v)]
assocs (KdMap _ t _) = go (Just t) []
  where go Nothing = id
        go (Just (TreeNode l p r)) = go l . (p :) . go r

keys :: KdMap k v -> [k]
keys = map fst . assocs

values :: KdMap k v -> [v]
values = map snd . assocs

isTreeValid :: KdSpace k -> Int -> TreeNode k v -> Bool
isTreeValid s axis (TreeNode l (p, _) r) =
  let xAxisVal = _coord s axis p
      nodeKey (TreeNode _ (p', _) _) = p'
      nextAxis = incrementAxis s axis
      leftChildValid = maybe True ((<= xAxisVal) . _coord s axis . nodeKey) l
      rightChildValid = maybe True ((> xAxisVal) . _coord s axis . nodeKey) r
      leftSubtreeValid = maybe True (isTreeValid s nextAxis) l
      rightSubtreeValid = maybe True (isTreeValid s nextAxis) r
  in  leftChildValid && rightChildValid && leftSubtreeValid && rightSubtreeValid

nearestNeighbor :: KdMap k v -> k -> (k, v)
nearestNeighbor (KdMap s t@(TreeNode _ root _) _) query =
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
          currDist       = _distSqr s query curr_p
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

nearNeighbors :: KdMap k v -> Double -> k -> [(k, v)]
nearNeighbors (KdMap s t _) radius query = go 0 t
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
              currentNear = if _distSqr s p query <= radius * radius
                            then [(p, d)]
                            else []
          in  onsideNear ++ currentNear ++ offsideNear

kNearestNeighbors :: KdMap k v -> Int -> k -> [(k, v)]
kNearestNeighbors (KdMap s t _) k query = reverse $ map snd $ Q.toList $ go 0 Q.empty t
  where -- go :: Int -> Q.MaxPQueue Double (p, d) -> TreeNode p d -> KQueue p d
        go axis q (TreeNode maybeLeft (p, d) maybeRight) =
          let queryAxisValue = _coord s axis query
              xAxisValue = _coord s axis p
              insertBounded queue dist x
                | Q.size queue < k = Q.insert dist x queue
                | otherwise = if dist < fst (Q.findMax queue)
                              then Q.deleteMax $ Q.insert dist x queue
                              else queue
              q' = insertBounded q (_distSqr s p query) (p, d)
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

size :: KdMap k v -> Int
size (KdMap _ _ n) = n

data Point2d = Point2d Double Double deriving (Show, Eq, Ord, Generic)
instance NFData Point2d where rnf = genericRnf

mk2DEuclideanSpace :: KdSpace Point2d
mk2DEuclideanSpace = KdSpace
                     { _numDimensions = 2
                     , _coord = coord
                     , _distSqr = dist
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

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps $ [0 ..]

checkValidTree :: KdSpace k -> [k] -> Bool
checkValidTree s ps =
  let (KdMap _ t _) = buildKdMap s $ testElements ps
  in  isTreeValid s 0 t

prop_validTree :: Property
prop_validTree = forAll (listOf1 arbitrary) $ checkValidTree mk2DEuclideanSpace

checkElements :: (Ord k, Show k) => KdSpace k -> [k] -> Bool
checkElements s ps =
  let kdt = buildKdMap s $ testElements ps
  in  L.sort (assocs kdt) == L.sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements mk2DEuclideanSpace

checkNumElements :: KdSpace k -> [k] -> Bool
checkNumElements s ps =
  let (KdMap _ _ n) = buildKdMap s $ testElements ps
  in  n == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements mk2DEuclideanSpace

nearestNeighborLinear :: KdSpace k -> [(k, v)] -> k -> (k, v)
nearestNeighborLinear s xs query =
  L.minimumBy (compare `on` (_distSqr s query . fst)) xs

checkNearestEqualToLinear :: Eq k => KdSpace k -> ([k], k) -> Bool
checkNearestEqualToLinear s (ps, query) =
  let kdt = buildKdMap s $ testElements ps
  in  nearestNeighbor kdt query == nearestNeighborLinear s (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear mk2DEuclideanSpace (xs, query)

nearNeighborsLinear :: KdSpace k -> [(k, v)] -> k -> Double -> [(k, v)]
nearNeighborsLinear s xs query radius =
  filter ((<= radius * radius) . _distSqr s query . fst) xs

checkNearEqualToLinear :: Ord k => KdSpace k -> Double -> ([k], k) -> Bool
checkNearEqualToLinear s radius (ps, query) =
  let kdt = buildKdMap s $ testElements ps
      kdtNear = nearNeighbors kdt radius query
      linearNear = nearNeighborsLinear s (testElements ps) query radius
  in  L.sort kdtNear == L.sort linearNear

prop_nearEqualToLinear :: Point2d -> Property
prop_nearEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkNearEqualToLinear mk2DEuclideanSpace radius (xs, query)

kNearestNeighborsLinear :: KdSpace k -> [(k, v)] -> k -> Int -> [(k, v)]
kNearestNeighborsLinear s xs query k =
  take k $ L.sortBy (compare `on` (_distSqr s query . fst)) xs

checkKNearestEqualToLinear :: Ord k => KdSpace k -> Int -> ([k], k) -> Bool
checkKNearestEqualToLinear s k (xs, query) =
  let kdt = buildKdMap s $ testElements xs
      kdtKNear = kNearestNeighbors kdt k query
      linearKNear = kNearestNeighborsLinear s (testElements xs) query k
  in  kdtKNear == linearKNear

prop_kNearestEqualToLinear :: Point2d -> Property
prop_kNearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToLinear mk2DEuclideanSpace k (xs, query)

checkKNearestSorted :: Eq k => KdSpace k -> ([k], k) -> Bool
checkKNearestSorted _ ([], _) = True
checkKNearestSorted s (ps, query) =
  let kdt = buildKdMap s $ testElements ps
      kNearestDists = map (_distSqr s query . fst) $ kNearestNeighbors kdt (length ps) query
  in  kNearestDists == L.sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted mk2DEuclideanSpace (xs, query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll
