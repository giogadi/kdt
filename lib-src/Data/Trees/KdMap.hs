{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.KdMap
       ( PointAsListFn
       , SquaredDistanceFn
       , KdMap
       , buildKdMap
       , buildKdMapWithDistSqrFn
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , size
       , assocs
       , keys
       , values
       , foldrKdMap
       , Point2d (..)
       , pointAsList2d
       , distSqr2d
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

data TreeNode k v = TreeNode { _treeLeft :: Maybe (TreeNode k v)
                             , _treePoint :: (k, v)
                             , _axisValue :: Double
                             , _treeRight :: Maybe (TreeNode k v)
                             }
                             deriving Generic
instance (NFData k, NFData v) => NFData (TreeNode k v) where rnf = genericRnf

mapTreeNode :: (v1 -> v2) -> TreeNode k v1 -> TreeNode k v2
mapTreeNode f (TreeNode maybeLeft (k, v) axisValue maybeRight) =
  TreeNode (fmap (mapTreeNode f) maybeLeft) (k, f v) axisValue (fmap (mapTreeNode f) maybeRight)

type PointAsListFn k = k -> [Double]

type SquaredDistanceFn k = k -> k -> Double

data KdMap k v = KdMap { _pointAsList :: PointAsListFn k
                       , _distSqr     :: SquaredDistanceFn k
                       , _rootNode    :: TreeNode k v
                       , _size        :: Int
                       } deriving Generic
instance (NFData k, NFData v) => NFData (KdMap k v) where rnf = genericRnf

instance Functor (KdMap k) where
  fmap f kdMap = kdMap { _rootNode = (mapTreeNode f (_rootNode kdMap)) }

foldrTreeNode :: ((k, v) -> a -> a) -> a -> TreeNode k v -> a
foldrTreeNode f z (TreeNode Nothing p _ Nothing) = f p z
foldrTreeNode f z (TreeNode (Just l) p _ Nothing) = foldrTreeNode f (f p z) l
foldrTreeNode f z (TreeNode Nothing p _ (Just r)) = f p (foldrTreeNode f z r)
foldrTreeNode f z (TreeNode (Just l) p _ (Just r)) =
  foldrTreeNode f (f p (foldrTreeNode f z r)) l

foldrKdMap :: ((k, v) -> a -> a) -> a -> KdMap k v -> a
foldrKdMap f z (KdMap _ _ r _) = foldrTreeNode f z r

instance Foldable (KdMap k) where
  foldr f = foldrKdMap (f . snd)

quickselect :: (a -> a -> Ordering) -> Int -> [a] -> a
quickselect cmp = go
  where go _ [] = error "quickselect must be called on a non-empty list."
        go k (x:xs) | k < l = go k ys
                    | k > l = go (k - l - 1) zs
                    | otherwise = x
          where (ys, zs) = L.partition ((== LT) . (`cmp` x)) xs
                l = length ys

buildKdMapWithDistSqrFn :: PointAsListFn k -> SquaredDistanceFn k -> [(k, v)] -> KdMap k v
buildKdMapWithDistSqrFn _ _ [] = error "KdMap must be built with a non-empty list."
buildKdMapWithDistSqrFn pointAsList distSqr points =
  let axisValsPointsPairs = zip (map (cycle . pointAsList . fst) points) points
  in  KdMap { _pointAsList = pointAsList
            , _distSqr     = distSqr
            , _rootNode    = buildTreeInternal axisValsPointsPairs
            , _size        = length points
            }
  where buildTreeInternal [(v : _, p)] = TreeNode Nothing p v Nothing
        buildTreeInternal ps =
          let n = length ps
              (medianAxisVal : _, medianPt) =
                quickselect (comparing (head . fst)) (n `div` 2) ps
              f ([], _) _ = error "buildKdMap.f: no empty lists allowed!"
              -- TODO: Doesn't this mess up if more than one point has
              -- the same axis value?
              f (v : vt, p) (lt, gt) | v < medianAxisVal = ((vt, p) : lt, gt)
                                     | v > medianAxisVal = (lt, (vt, p) : gt)
                                     | otherwise = (lt, gt)
              (leftPoints, rightPoints) = L.foldr f ([], []) ps
              maybeBuildTree [] = Nothing
              maybeBuildTree ps' = Just $ buildTreeInternal ps'
          in  TreeNode
              { _treeLeft  = maybeBuildTree leftPoints
              , _treePoint = medianPt
              , _axisValue = medianAxisVal
              , _treeRight = maybeBuildTree rightPoints
              }

defaultDistSqrFn :: PointAsListFn k -> SquaredDistanceFn k
defaultDistSqrFn pointAsList k1 k2 =
  L.sum $ map (^ (2 :: Int)) $ zipWith (-) (pointAsList k1) (pointAsList k2)

buildKdMap :: PointAsListFn k -> [(k, v)] -> KdMap k v
buildKdMap pointAsList =
  buildKdMapWithDistSqrFn pointAsList $ defaultDistSqrFn pointAsList

assocsInternal :: TreeNode k v -> [(k, v)]
assocsInternal t = go t []
  where go (TreeNode l p _ r) =
          maybe id go l . (p :) . maybe id go r

assocs :: KdMap k v -> [(k, v)]
assocs (KdMap _ _ t _) = assocsInternal t

keys :: KdMap k v -> [k]
keys = map fst . assocs

values :: KdMap k v -> [v]
values = map snd . assocs

nearestNeighbor :: KdMap k v -> k -> (k, v)
nearestNeighbor (KdMap pointAsList distSqr t@(TreeNode _ root _ _) _) query =
  -- This is an ugly way to kickstart the function but it's faster
  -- than using a Maybe.
  fst $ go (root, 1 / 0 :: Double) (cycle $ pointAsList query) t
  where
    go _ [] _ = error "nearestNeighbor.go: no empty lists allowed!"
    go bestSoFar (queryAxisValue : qvs) (TreeNode maybeLeft (curr_p, curr_d) curr_v maybeRight) =
      let better (x1, dist1) (x2, dist2) = if dist1 < dist2
                                           then (x1, dist1)
                                           else (x2, dist2)
          currDist       = distSqr query curr_p
          bestAfterCurr = better ((curr_p, curr_d), currDist) bestSoFar
          nearestInTree maybeOnsideTree maybeOffsideTree =
            let bestAfterOnside =
                  maybe bestAfterCurr (go bestAfterCurr qvs) maybeOnsideTree
                checkOffsideTree =
                  (queryAxisValue - curr_v)^(2 :: Int) < snd bestAfterOnside
            in  if checkOffsideTree
                then maybe bestAfterOnside (go bestAfterOnside qvs) maybeOffsideTree
                else bestAfterOnside
      in  if queryAxisValue <= curr_v
          then nearestInTree maybeLeft maybeRight
          else nearestInTree maybeRight maybeLeft

nearNeighbors :: KdMap k v -> Double -> k -> [(k, v)]
nearNeighbors (KdMap pointAsList distSqr t _) radius query = go (cycle $ pointAsList query) t
  where
    go [] _ = error "nearNeighbors.go: no empty lists allowed!"
    go (queryAxisValue : qvs) (TreeNode maybeLeft (p, d) xAxisValue maybeRight) =
      let nears = maybe [] (go qvs)
          onTheLeft = queryAxisValue <= xAxisValue
          onsideNear = if onTheLeft
                       then nears maybeLeft
                       else nears maybeRight
          offsideNear = if abs (queryAxisValue - xAxisValue) < radius
                        then if onTheLeft
                             then nears maybeRight
                             else nears maybeLeft
                        else []
          currentNear = if distSqr p query <= radius * radius
                        then [(p, d)]
                        else []
      in  onsideNear ++ currentNear ++ offsideNear

kNearestNeighbors :: KdMap k v -> Int -> k -> [(k, v)]
kNearestNeighbors (KdMap pointAsList distSqr t _) k query =
  reverse $ map snd $ Q.toList $ go (cycle $ pointAsList query) Q.empty t
  where
    -- go :: [Double] -> Q.MaxPQueue Double (p, d) -> TreeNode p d -> KQueue p d
    go [] _ _ = error "kNearestNeighbors.go: no empty lists allowed!"
    go (queryAxisValue : qvs) q (TreeNode maybeLeft (p, d) xAxisValue maybeRight) =
      let insertBounded queue dist x
            | Q.size queue < k = Q.insert dist x queue
            | otherwise = if dist < fst (Q.findMax queue)
                          then Q.deleteMax $ Q.insert dist x queue
                          else queue
          q' = insertBounded q (distSqr p query) (p, d)
          kNearest queue maybeOnsideTree maybeOffsideTree =
            let queue' = maybe queue (go qvs queue) maybeOnsideTree
                checkOffsideTree =
                  Q.size queue' < k ||
                  (queryAxisValue - xAxisValue)^(2 :: Int) < fst (Q.findMax queue')
            in  if checkOffsideTree
                then maybe queue' (go qvs queue') maybeOffsideTree
                else queue'
      in  if queryAxisValue <= xAxisValue
          then kNearest q' maybeLeft maybeRight
          else kNearest q' maybeRight maybeLeft

size :: KdMap k v -> Int
size (KdMap _ _ _ n) = n

--------------------------------------------------------------------------------
-- Example: Point2d
--------------------------------------------------------------------------------

data Point2d = Point2d Double Double deriving (Show, Eq, Ord, Generic)
instance NFData Point2d where rnf = genericRnf

pointAsList2d :: Point2d -> [Double]
pointAsList2d (Point2d x y) = [x, y]

distSqr2d :: Point2d -> Point2d -> Double
distSqr2d (Point2d x1 y1) (Point2d x2 y2) = let dx = x2 - x1
                                                dy = y2 - y1
                                            in  dx*dx + dy*dy

instance Arbitrary Point2d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Point2d x y)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [0 ..]

isTreeValid :: PointAsListFn k -> Int -> TreeNode k v -> Bool
isTreeValid pointAsList axis (TreeNode l (p, _) xAxisVal r) =
  let nodeAxisVal (TreeNode _ (p', _) _ _) = pointAsList p' !! axis
      nextAxis = (axis + 1) `mod` length (pointAsList p)
      -- TODO: Shouldn't it be < and >=?
      leftChildValid = maybe True ((<= xAxisVal) . nodeAxisVal) l
      rightChildValid = maybe True ((> xAxisVal) . nodeAxisVal) r
      leftSubtreeValid = maybe True (isTreeValid pointAsList nextAxis) l
      rightSubtreeValid = maybe True (isTreeValid pointAsList nextAxis) r
  in  leftChildValid && rightChildValid && leftSubtreeValid && rightSubtreeValid

checkValidTree :: PointAsListFn k -> [k] -> Bool
checkValidTree pointAsList ps =
  let (KdMap _ _ r _) = buildKdMap pointAsList $ testElements ps
  in  isTreeValid pointAsList 0 r

prop_validTree :: Property
prop_validTree = forAll (listOf1 arbitrary) $ checkValidTree pointAsList2d

checkElements :: (Ord k, Show k) => PointAsListFn k -> [k] -> Bool
checkElements pointAsList ps =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  L.sort (assocs kdt) == L.sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements pointAsList2d

checkNumElements :: PointAsListFn k -> [k] -> Bool
checkNumElements pointAsList ps =
  let (KdMap _ _ _ n) = buildKdMap pointAsList $ testElements ps
  in  n == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements pointAsList2d

nearestNeighborLinear :: PointAsListFn k -> [(k, v)] -> k -> (k, v)
nearestNeighborLinear pointAsList xs query =
  L.minimumBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkNearestEqualToLinear :: Eq k => PointAsListFn k -> ([k], k) -> Bool
checkNearestEqualToLinear pointAsList (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  nearestNeighbor kdt query == nearestNeighborLinear pointAsList (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear pointAsList2d (xs, query)

nearNeighborsLinear :: PointAsListFn k -> [(k, v)] -> k -> Double -> [(k, v)]
nearNeighborsLinear pointAsList xs query radius =
  filter ((<= radius * radius) . defaultDistSqrFn pointAsList query . fst) xs

checkNearEqualToLinear :: Ord k => PointAsListFn k -> Double -> ([k], k) -> Bool
checkNearEqualToLinear pointAsList radius (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
      kdtNear = nearNeighbors kdt radius query
      linearNear = nearNeighborsLinear pointAsList (testElements ps) query radius
  in  L.sort kdtNear == L.sort linearNear

prop_nearEqualToLinear :: Point2d -> Property
prop_nearEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkNearEqualToLinear pointAsList2d radius (xs, query)

kNearestNeighborsLinear :: PointAsListFn k -> [(k, v)] -> k -> Int -> [(k, v)]
kNearestNeighborsLinear pointAsList xs query k =
  take k $ L.sortBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkKNearestEqualToLinear :: Ord k => PointAsListFn k -> Int -> ([k], k) -> Bool
checkKNearestEqualToLinear pointAsList k (xs, query) =
  let kdt = buildKdMap pointAsList $ testElements xs
      kdtKNear = kNearestNeighbors kdt k query
      linearKNear = kNearestNeighborsLinear pointAsList (testElements xs) query k
  in  kdtKNear == linearKNear

prop_kNearestEqualToLinear :: Point2d -> Property
prop_kNearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToLinear pointAsList2d k (xs, query)

checkKNearestSorted :: Eq k => PointAsListFn k -> ([k], k) -> Bool
checkKNearestSorted _ ([], _) = True
checkKNearestSorted pointAsList (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
      kNearestDists =
        map (defaultDistSqrFn pointAsList query . fst) $ kNearestNeighbors kdt (length ps) query
  in  kNearestDists == L.sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted pointAsList2d (xs, query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll
