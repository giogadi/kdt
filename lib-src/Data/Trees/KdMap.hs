{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

-- TODO: Implement range find?
module Data.Trees.KdMap
       ( PointAsListFn
       , SquaredDistanceFn
       , defaultDistSqrFn
       , KdMap
       , buildKdMap
       , buildKdMapWithDistFn
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , size
       , assocs
       , keys
       , values
       , foldrKdMap
       , runTests
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import Control.Applicative
import Data.Foldable
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Ord
import qualified Data.PQueue.Prio.Max as Q
import Data.Traversable
import Test.QuickCheck

import Data.Point2d

data TreeNode a k v = TreeNode { _treeLeft :: TreeNode a k v
                               , _treePoint :: (k, v)
                               , _axisValue :: a
                               , _treeRight :: TreeNode a k v
                               } |
                      Empty
  deriving Generic
instance (NFData k, NFData a, NFData v) => NFData (TreeNode a k v) where rnf = genericRnf

mapTreeNode :: (v1 -> v2) -> TreeNode a k v1 -> TreeNode a k v2
mapTreeNode _ Empty = Empty
mapTreeNode f (TreeNode left (k, v) axisValue right) =
  TreeNode (mapTreeNode f left) (k, f v) axisValue (mapTreeNode f right)

type PointAsListFn a k = k -> [a]

type SquaredDistanceFn a k = k -> k -> a

data KdMap a k v = KdMap { _pointAsList :: PointAsListFn a k
                         , _distSqr     :: SquaredDistanceFn a k
                         , _rootNode    :: TreeNode a k v
                         , _size        :: Int
                         } deriving Generic
instance (NFData k, NFData a, NFData v) => NFData (KdMap a k v) where rnf = genericRnf

instance Functor (KdMap a k) where
  fmap f kdMap = kdMap { _rootNode = (mapTreeNode f (_rootNode kdMap)) }

foldrTreeNode :: ((k, v) -> b -> b) -> b -> TreeNode a k v -> b
foldrTreeNode _ z Empty = z
foldrTreeNode f z (TreeNode left p _ right) =
  foldrTreeNode f (f p (foldrTreeNode f z right)) left

foldrKdMap :: ((k, v) -> b -> b) -> b -> KdMap a k v -> b
foldrKdMap f z (KdMap _ _ r _) = foldrTreeNode f z r

instance Foldable (KdMap a k) where
  foldr f = foldrKdMap (f . snd)

traverseTreeNode :: Applicative f => (b -> f c) -> TreeNode a k b -> f (TreeNode a k c)
traverseTreeNode _ Empty = pure Empty
traverseTreeNode f (TreeNode l p axisValue r) =
  TreeNode <$>
    traverseTreeNode f l <*>
    traverse f p <*>
    pure axisValue <*>
    traverseTreeNode f r

instance Traversable (KdMap a k) where
  traverse f (KdMap p d r n) =
    KdMap <$> pure p <*> pure d <*> traverseTreeNode f r <*> pure n

quickselect :: (b -> b -> Ordering) -> Int -> [b] -> b
quickselect cmp = go
  where go _ [] = error "quickselect must be called on a non-empty list."
        go k (x:xs) | k < l = go k ys
                    | k > l = go (k - l - 1) zs
                    | otherwise = x
          where (ys, zs) = L.partition ((== LT) . (`cmp` x)) xs
                l = length ys

buildKdMapWithDistFn :: Real a => PointAsListFn a k ->
                                  SquaredDistanceFn a k ->
                                  [(k, v)] ->
                                  KdMap a k v
buildKdMapWithDistFn _ _ [] = error "KdMap must be built with a non-empty list."
buildKdMapWithDistFn pointAsList distSqr points =
  let axisValsPointsPairs = zip (map (cycle . pointAsList . fst) points) points
  in  KdMap { _pointAsList = pointAsList
            , _distSqr     = distSqr
            , _rootNode    = buildTreeInternal axisValsPointsPairs
            , _size        = length points
            }
  where buildTreeInternal [] = Empty
        buildTreeInternal ps =
          let n = length ps
              (medianAxisVal : _, _) =
                quickselect (comparing (head . fst)) (n `div` 2) ps
              f ([], _) _ = error "buildKdMap.f: no empty lists allowed!"
              f (v : vt, p) (lt, maybeMedian, gt)
                | v < medianAxisVal = ((vt, p) : lt, maybeMedian, gt)
                | v > medianAxisVal = (lt, maybeMedian, (vt, p) : gt)
                | otherwise =
                    case maybeMedian of
                      Nothing -> (lt, Just p, gt)
                      Just _ -> ((vt, p) : lt, maybeMedian, gt)
              (leftPoints, maybeMedianPt, rightPoints) = L.foldr f ([], Nothing, []) ps
          in  TreeNode
              { _treeLeft  = buildTreeInternal leftPoints
              , _treePoint = fromJust maybeMedianPt
              , _axisValue = medianAxisVal
              , _treeRight = buildTreeInternal rightPoints
              }

defaultDistSqrFn :: Num a => PointAsListFn a k -> SquaredDistanceFn a k
defaultDistSqrFn pointAsList k1 k2 =
  L.sum $ map (^ (2 :: Int)) $ zipWith (-) (pointAsList k1) (pointAsList k2)

buildKdMap :: Real a => PointAsListFn a k -> [(k, v)] -> KdMap a k v
buildKdMap pointAsList =
  buildKdMapWithDistFn pointAsList $ defaultDistSqrFn pointAsList

assocsInternal :: TreeNode a k v -> [(k, v)]
assocsInternal t = go t []
  where go Empty = id
        go (TreeNode l p _ r) = go l . (p :) . go r

assocs :: KdMap a k v -> [(k, v)]
assocs (KdMap _ _ t _) = assocsInternal t

keys :: KdMap a k v -> [k]
keys = map fst . assocs

values :: KdMap a k v -> [v]
values = map snd . assocs

nearestNeighbor :: Real a => KdMap a k v -> k -> (k, v)
nearestNeighbor (KdMap _ _ Empty _) _ =
  error "nearestNeighbor: why is there an empty KdMap?"
nearestNeighbor (KdMap pointAsList distSqr t@(TreeNode _ root _ _) _) query =
  -- This is an ugly way to kickstart the function but it's faster
  -- than using a Maybe.
  fst $ go (root, distSqr (fst root) query) (cycle $ pointAsList query) t
  where
    go _ [] _ = error "nearestNeighbor.go: no empty lists allowed!"
    go bestSoFar _ Empty = bestSoFar
    go bestSoFar
       (queryAxisValue : qvs)
       (TreeNode left (nodeK, nodeV) nodeAxisVal right) =
      let better x1@(_, dist1) x2@(_, dist2) = if dist1 < dist2
                                               then x1
                                               else x2
          currDist       = distSqr query nodeK
          bestAfterNode = better ((nodeK, nodeV), currDist) bestSoFar
          nearestInTree onsideSubtree offsideSubtree =
            let bestAfterOnside = go bestAfterNode qvs onsideSubtree
                checkOffsideSubtree =
                  (queryAxisValue - nodeAxisVal)^(2 :: Int) < snd bestAfterOnside
            in  if checkOffsideSubtree
                then go bestAfterOnside qvs offsideSubtree
                else bestAfterOnside
      in  if queryAxisValue <= nodeAxisVal
          then nearestInTree left right
          else nearestInTree right left

nearNeighbors :: Real a => KdMap a k v -> a -> k -> [(k, v)]
nearNeighbors (KdMap pointAsList distSqr t _) radius query =
  go (cycle $ pointAsList query) t
  where
    go [] _ = error "nearNeighbors.go: no empty lists allowed!"
    go _ Empty = []
    go (queryAxisValue : qvs) (TreeNode left (k, v) nodeAxisVal right) =
      let onTheLeft = queryAxisValue <= nodeAxisVal
          onsideNear = if   onTheLeft
                       then go qvs left
                       else go qvs right
          offsideNear = if   abs (queryAxisValue - nodeAxisVal) < radius
                        then if   onTheLeft
                             then go qvs right
                             else go qvs left
                        else []
          currentNear = if distSqr k query <= radius * radius
                        then [(k, v)]
                        else []
      in  onsideNear ++ currentNear ++ offsideNear

kNearestNeighbors :: Real a => KdMap a k v -> Int -> k -> [(k, v)]
kNearestNeighbors (KdMap pointAsList distSqr t _) numNeighbors query =
  reverse $ map snd $ Q.toList $ go (cycle $ pointAsList query) Q.empty t
  where
    -- go :: [Double] -> Q.MaxPQueue Double (p, d) -> TreeNode p d -> KQueue p d
    go [] _ _ = error "kNearestNeighbors.go: no empty lists allowed!"
    go _ q Empty = q
    go (queryAxisValue : qvs) q (TreeNode left (k, v) nodeAxisVal right) =
      let insertBounded queue dist x
            | Q.size queue < numNeighbors = Q.insert dist x queue
            | otherwise = if dist < fst (Q.findMax queue)
                          then Q.deleteMax $ Q.insert dist x queue
                          else queue
          q' = insertBounded q (distSqr k query) (k, v)
          kNearest queue onsideSubtree offsideSubtree =
            let queue' = go qvs queue onsideSubtree
                checkOffsideTree =
                  Q.size queue' < numNeighbors ||
                  (queryAxisValue - nodeAxisVal)^(2 :: Int) < fst (Q.findMax queue')
            in  if checkOffsideTree
                then go qvs queue' offsideSubtree
                else queue'
      in  if queryAxisValue <= nodeAxisVal
          then kNearest q' left right
          else kNearest q' right left

size :: KdMap a k v -> Int
size (KdMap _ _ _ n) = n

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [0 ..]

isTreeValid :: Real a => PointAsListFn a k -> Int -> TreeNode a k v -> Bool
isTreeValid _ _ Empty = True
isTreeValid pointAsList axis (TreeNode l (k, _) nodeAxisVal r) =
  let childrenAxisValues = map ((!! axis) . pointAsList . fst) . assocsInternal
      leftSubtreeLess = L.all (<= nodeAxisVal) $ childrenAxisValues l
      rightSubtreeGreater = L.all (> nodeAxisVal) $ childrenAxisValues r
      nextAxis = (axis + 1) `mod` length (pointAsList k)
  in  leftSubtreeLess && rightSubtreeGreater &&
      isTreeValid pointAsList nextAxis l && isTreeValid pointAsList nextAxis r

checkValidTree :: Real a => PointAsListFn a k -> [k] -> Bool
checkValidTree pointAsList ps =
  let (KdMap _ _ r _) = buildKdMap pointAsList $ testElements ps
  in  isTreeValid pointAsList 0 r

prop_validTree :: Property
prop_validTree = forAll (listOf1 arbitrary) $ checkValidTree pointAsList2d

checkElements :: (Ord k, Real a) => PointAsListFn a k -> [k] -> Bool
checkElements pointAsList ps =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  L.sort (assocs kdt) == L.sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements pointAsList2d

checkNumElements :: Real a => PointAsListFn a k -> [k] -> Bool
checkNumElements pointAsList ps =
  let (KdMap _ _ _ n) = buildKdMap pointAsList $ testElements ps
  in  n == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements pointAsList2d

nearestNeighborLinear :: Real a => PointAsListFn a k -> [(k, v)] -> k -> (k, v)
nearestNeighborLinear pointAsList xs query =
  L.minimumBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkNearestEqualToLinear :: (Eq k, Real a) => PointAsListFn a k -> ([k], k) -> Bool
checkNearestEqualToLinear pointAsList (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  nearestNeighbor kdt query == nearestNeighborLinear pointAsList (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear pointAsList2d (xs, query)

nearNeighborsLinear :: Real a => PointAsListFn a k -> [(k, v)] -> k -> a -> [(k, v)]
nearNeighborsLinear pointAsList xs query radius =
  filter ((<= radius * radius) . defaultDistSqrFn pointAsList query . fst) xs

checkNearEqualToLinear :: (Ord k, Real a) => PointAsListFn a k -> a -> ([k], k) -> Bool
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

kNearestNeighborsLinear :: Real a => PointAsListFn a k -> [(k, v)] -> k -> Int -> [(k, v)]
kNearestNeighborsLinear pointAsList xs query k =
  take k $ L.sortBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkKNearestEqualToLinear :: (Ord k, Real a) => PointAsListFn a k -> Int -> ([k], k) -> Bool
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

checkKNearestSorted :: (Eq k, Real a) => PointAsListFn a k -> ([k], k) -> Bool
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

prop_equalAxisValueSameElems :: Property
prop_equalAxisValueSameElems =
  forAll (listOf1 arbitrary) $ \xs@((Point2d x y) : _) ->
    checkElements pointAsList2d $ (Point2d x (y + 1)) : xs

prop_equalAxisValueEqualToLinear :: Point2d -> Property
prop_equalAxisValueEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs@((Point2d x y) : _) ->
    checkNearestEqualToLinear pointAsList2d (((Point2d x (y + 1)) : xs), query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll
