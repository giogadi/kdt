{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.KdMap.Static
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdMap
         -- ** /k/-d map construction
       , buildKdMap
       , buildKdMapWithDistFn
         -- ** Query
       , nearestNeighbor
       , pointsInRadius
       , kNearestNeighbors
       , pointsInRange
       , assocs
       , points
       , values
       , size
         -- ** Folds
       , foldrKdMap
         -- ** Utilities
       , defaultDistSqrFn
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

-- $usage
--
-- The 'KdMap' is a variant of 'Data.KdTree.Static.KdTree' where each point in
-- the tree is associated with some data. When talking about 'KdMap's,
-- we'll refer to the points and their associated data as the /points/
-- and /values/ of the 'KdMap', respectively. It might help to think
-- of 'Data.KdTree.Static.KdTree' and 'KdMap' as being analogous to
-- 'Data.Set' and 'Data.Map'.
--
-- Suppose you wanted to perform point queries on a set of 3D points,
-- where each point is associated with a 'String'. Here's how to build
-- a 'KdMap' of the data and perform a nearest neighbor query (if this
-- doesn't make sense, start with the documentation for
-- 'Data.KdTree.Static.KdTree'):
--
-- @
-- >>> let points = [(Point3d 0.0 0.0 0.0), (Point3d 1.0 1.0 1.0)]
--
-- >>> let valueStrings = [\"First\", \"Second\"]
--
-- >>> let pointValuePairs = zip points valueStrings
--
-- >>> let kdm = buildKdMap point3dAsList pointValuePairs
--
-- >>> nearestNeighbor kdm (Point3d 0.1 0.1 0.1)
-- [Point3d {x = 0.0, y = 0.0, z = 0.0}, \"First\"]
-- @

data TreeNode a p v = TreeNode { _treeLeft :: TreeNode a p v
                               , _treePoint :: (p, v)
                               , _axisValue :: a
                               , _treeRight :: TreeNode a p v
                               } |
                      Empty
  deriving Generic
instance (NFData a, NFData p, NFData v) => NFData (TreeNode a p v) where rnf = genericRnf

mapTreeNode :: (v1 -> v2) -> TreeNode a p v1 -> TreeNode a p v2
mapTreeNode _ Empty = Empty
mapTreeNode f (TreeNode left (k, v) axisValue right) =
  TreeNode (mapTreeNode f left) (k, f v) axisValue (mapTreeNode f right)

-- | Converts a point of type @p@ with axis values of type
-- @a@ into a list of axis values [a].
type PointAsListFn a p = p -> [a]

-- | Returns the squared distance between two points of type
-- @p@ with axis values of type @a@.
type SquaredDistanceFn a p = p -> p -> a

-- | A /k/-d tree structure that stores points of type @p@ with axis
-- values of type @a@. Additionally, each point is associated with a
-- value of type @v@.
data KdMap a p v = KdMap { _pointAsList :: PointAsListFn a p
                         , _distSqr     :: SquaredDistanceFn a p
                         , _rootNode    :: TreeNode a p v
                         , _size        :: Int
                         } deriving Generic
instance (NFData a, NFData p, NFData v) => NFData (KdMap a p v) where rnf = genericRnf

instance Functor (KdMap a p) where
  fmap f kdMap = kdMap { _rootNode = mapTreeNode f (_rootNode kdMap) }

foldrTreeNode :: ((p, v) -> b -> b) -> b -> TreeNode a p v -> b
foldrTreeNode _ z Empty = z
foldrTreeNode f z (TreeNode left p _ right) =
  foldrTreeNode f (f p (foldrTreeNode f z right)) left

-- | Performs a foldr over each point-value pair in the 'KdMap'.
foldrKdMap :: ((p, v) -> b -> b) -> b -> KdMap a p v -> b
foldrKdMap f z (KdMap _ _ r _) = foldrTreeNode f z r

instance Foldable (KdMap a p) where
  foldr f = foldrKdMap (f . snd)

traverseTreeNode :: Applicative f => (b -> f c) -> TreeNode a p b -> f (TreeNode a p c)
traverseTreeNode _ Empty = pure Empty
traverseTreeNode f (TreeNode l (p, v) axisValue r) =
  TreeNode <$>
    traverseTreeNode f l <*>
    ((,) p <$> f v) <*> -- would simply be traverse f (p, v), but
                        -- base-4.6.* doesn't have a Traversable
                        -- instance for tuples.
    pure axisValue <*>
    traverseTreeNode f r

instance Traversable (KdMap a p) where
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

-- | Builds a 'KdMap' from a list of pairs of points (of type p) and
-- values (of type v), using a user-specified squared distance
-- function.
--
-- Average time complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
--
-- Throws an error if given an empty list of data points.
buildKdMapWithDistFn :: Real a => PointAsListFn a p ->
                                  SquaredDistanceFn a p ->
                                  [(p, v)] ->
                                  KdMap a p v
buildKdMapWithDistFn _ _ [] = error "KdMap must be built with a non-empty list."
buildKdMapWithDistFn pointAsList distSqr dataPoints =
  let axisValsPointsPairs = zip (map (cycle . pointAsList . fst) dataPoints) dataPoints
  in  KdMap { _pointAsList = pointAsList
            , _distSqr     = distSqr
            , _rootNode    = buildTreeInternal axisValsPointsPairs
            , _size        = length dataPoints
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

-- | A default implementation of squared distance given two points and
-- a 'PointAsListFn'.
defaultDistSqrFn :: Num a => PointAsListFn a p -> SquaredDistanceFn a p
defaultDistSqrFn pointAsList k1 k2 =
  L.sum $ map (^ (2 :: Int)) $ zipWith (-) (pointAsList k1) (pointAsList k2)

-- | Builds a 'KdTree' from a list of pairs of points (of type p) and
-- values (of type v) using a default squared distance function
-- 'defaultDistSqrFn'.
--
-- Average complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
--
-- Throws an error if given an empty list of data points.
buildKdMap :: Real a => PointAsListFn a p -> [(p, v)] -> KdMap a p v
buildKdMap pointAsList =
  buildKdMapWithDistFn pointAsList $ defaultDistSqrFn pointAsList

assocsInternal :: TreeNode a p v -> [(p, v)]
assocsInternal t = go t []
  where go Empty = id
        go (TreeNode l p _ r) = go l . (p :) . go r

-- | Returns a list of all the point-value pairs in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
assocs :: KdMap a p v -> [(p, v)]
assocs (KdMap _ _ t _) = assocsInternal t

-- | Returns all points in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
points :: KdMap a p v -> [p]
points = map fst . assocs

-- | Returns all values in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
values :: KdMap a p v -> [v]
values = map snd . assocs

-- | Given a 'KdMap' and a query point, returns the point-value pair
-- in the 'KdMap' with the point nearest to the query.
--
-- Average time complexity: /O(log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
nearestNeighbor :: Real a => KdMap a p v -> p -> (p, v)
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

-- | Given a 'KdMap', a query point, and a radius, returns all
-- point-value pairs in the 'KdMap' with points within the given
-- radius of the query point.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for /n/ data points and a radius
-- that spans all points in the structure.
pointsInRadius :: Real a => KdMap a p v
                            -> a -- ^ radius
                            -> p -- ^ query point
                            -> [(p, v)] -- ^ list of point-value pairs
                                        -- with points within given
                                        -- radius of query
pointsInRadius (KdMap pointAsList distSqr t _) radius query =
  go (cycle $ pointAsList query) t []
  where
    go [] _ _ = error "pointsInRadius.go: no empty lists allowed!"
    go _ Empty acc = acc
    go (queryAxisValue : qvs) (TreeNode left (k, v) nodeAxisVal right) acc =
      let onTheLeft = queryAxisValue <= nodeAxisVal
          accAfterOnside = if   onTheLeft
                           then go qvs left acc
                           else go qvs right acc
          accAfterOffside = if   abs (queryAxisValue - nodeAxisVal) < radius
                            then if   onTheLeft
                                 then go qvs right accAfterOnside
                                 else go qvs left accAfterOnside
                            else accAfterOnside
          accAfterCurrent = if distSqr k query <= radius * radius
                            then (k, v) : accAfterOffside
                            else accAfterOffside
      in  accAfterCurrent

-- | Given a 'KdMap', a query point, and a number @k@, returns the @k@
-- point-value pairs with the nearest points to the query.
--
-- Neighbors are returned in order of increasing distance from query
-- point.
--
-- Average time complexity: /log(k) * log(n)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
--
-- Worst case time complexity: /n * log(k)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
kNearestNeighbors :: Real a => KdMap a p v -> Int -> p -> [(p, v)]
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
                          then Q.insert dist x $ Q.deleteMax queue
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

-- | Finds all point-value pairs in a 'KdMap' with points within a
-- given range, where the range is specified as a set of lower and
-- upper bounds.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for n data points and a range
-- that spans all the points.
--
-- TODO: Maybe use known bounds on entire tree structure to be able to
-- automatically count whole portions of tree as being within given
-- range.
pointsInRange :: Real a => KdMap a p v
                           -> p -- ^ lower bounds of range
                           -> p -- ^ upper bounds of range
                           -> [(p, v)] -- ^ point-value pairs within
                                       -- given range
pointsInRange (KdMap pointAsList _ t _) lowers uppers =
  go (cycle (pointAsList lowers) `zip` cycle (pointAsList uppers)) t []
  where
    go [] _ _ = error "neighborsInRange.go: no empty lists allowed!"
    go _ Empty acc = acc
    go ((lower, upper) : nextBounds) (TreeNode left p nodeAxisVal right) acc =
      let accAfterLeft = if lower <= nodeAxisVal
                         then go nextBounds left acc
                         else acc
          accAfterRight = if upper > nodeAxisVal
                          then go nextBounds right accAfterLeft
                          else accAfterLeft
          valInRange l x u = l <= x && x <= u
          -- maybe "cache" lowers and uppers as lists sooner as hint
          -- to ghc. Also, maybe only need to check previously
          -- unchecked axes?
          currentInRange =
            L.and $ zipWith3 valInRange
              (pointAsList lowers) (pointAsList $ fst p) (pointAsList uppers)
          accAfterCurrent = if currentInRange
                            then p : accAfterRight
                            else accAfterRight
      in  accAfterCurrent

-- | Returns the number of point-value pairs in the 'KdMap'.
--
-- Time complexity: /O(1)/
size :: KdMap a p v -> Int
size (KdMap _ _ _ n) = n

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [0 ..]

isTreeValid :: Real a => PointAsListFn a p -> Int -> TreeNode a p v -> Bool
isTreeValid _ _ Empty = True
isTreeValid pointAsList axis (TreeNode l (k, _) nodeAxisVal r) =
  let childrenAxisValues = map ((!! axis) . pointAsList . fst) . assocsInternal
      leftSubtreeLess = L.all (<= nodeAxisVal) $ childrenAxisValues l
      rightSubtreeGreater = L.all (> nodeAxisVal) $ childrenAxisValues r
      nextAxis = (axis + 1) `mod` length (pointAsList k)
  in  leftSubtreeLess && rightSubtreeGreater &&
      isTreeValid pointAsList nextAxis l && isTreeValid pointAsList nextAxis r

checkValidTree :: Real a => PointAsListFn a p -> [p] -> Bool
checkValidTree pointAsList ps =
  let (KdMap _ _ r _) = buildKdMap pointAsList $ testElements ps
  in  isTreeValid pointAsList 0 r

prop_validTree :: Property
prop_validTree = forAll (listOf1 arbitrary) $ checkValidTree pointAsList2d

checkElements :: (Ord p, Real a) => PointAsListFn a p -> [p] -> Bool
checkElements pointAsList ps =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  L.sort (assocs kdt) == L.sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements pointAsList2d

checkNumElements :: Real a => PointAsListFn a p -> [p] -> Bool
checkNumElements pointAsList ps =
  let (KdMap _ _ _ n) = buildKdMap pointAsList $ testElements ps
  in  n == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements pointAsList2d

nearestNeighborLinear :: Real a => PointAsListFn a p -> [(p, v)] -> p -> (p, v)
nearestNeighborLinear pointAsList xs query =
  L.minimumBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkNearestEqualToLinear :: (Eq p, Real a) => PointAsListFn a p -> ([p], p) -> Bool
checkNearestEqualToLinear pointAsList (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
  in  nearestNeighbor kdt query == nearestNeighborLinear pointAsList (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear pointAsList2d (xs, query)

pointsInRadiusLinear :: Real a => PointAsListFn a p -> [(p, v)] -> p -> a -> [(p, v)]
pointsInRadiusLinear pointAsList xs query radius =
  filter ((<= radius * radius) . defaultDistSqrFn pointAsList query . fst) xs

checkInRadiusEqualToLinear :: (Ord p, Real a) => PointAsListFn a p -> a -> ([p], p) -> Bool
checkInRadiusEqualToLinear pointAsList radius (ps, query) =
  let kdt = buildKdMap pointAsList $ testElements ps
      kdtNear = pointsInRadius kdt radius query
      linearNear = pointsInRadiusLinear pointAsList (testElements ps) query radius
  in  L.sort kdtNear == L.sort linearNear

prop_inRadiusEqualToLinear :: Point2d -> Property
prop_inRadiusEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkInRadiusEqualToLinear pointAsList2d radius (xs, query)

kNearestNeighborsLinear :: Real a => PointAsListFn a p -> [(p, v)] -> p -> Int -> [(p, v)]
kNearestNeighborsLinear pointAsList xs query k =
  take k $ L.sortBy (compare `on` (defaultDistSqrFn pointAsList query . fst)) xs

checkKNearestEqualToLinear :: (Ord p, Real a) => PointAsListFn a p -> Int -> ([p], p) -> Bool
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

checkKNearestSorted :: (Eq p, Real a) => PointAsListFn a p -> ([p], p) -> Bool
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

rangeLinear :: Real a => PointAsListFn a p -> [(p, v)] -> p -> p -> [(p, v)]
rangeLinear pointAsList xs lowers uppers =
  let valInRange a lower upper = lower <= a && a <= upper
      lowersAsList = pointAsList lowers
      uppersAsList = pointAsList uppers
      pointInRange (p, _) =
        L.and $ zipWith3 valInRange (pointAsList p) lowersAsList uppersAsList
  in  filter pointInRange xs

prop_rangeEqualToLinear :: ([Point2d], Point2d, Point2d) -> Bool
prop_rangeEqualToLinear (xs, lowers, uppers)
  | null xs = True
  | L.and $ zipWith (<) (pointAsList2d lowers) (pointAsList2d uppers) =
      let linear = rangeLinear pointAsList2d (testElements xs) lowers uppers
          kdt    = buildKdMap pointAsList2d $ testElements xs
          kdtPoints = pointsInRange kdt lowers uppers
      in  L.sort linear == L.sort kdtPoints
  | otherwise = True

prop_equalAxisValueSameElems :: Property
prop_equalAxisValueSameElems =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkElements pointAsList2d $ Point2d x (y + 1) : xs

prop_equalAxisValueEqualToLinear :: Point2d -> Property
prop_equalAxisValueEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkNearestEqualToLinear pointAsList2d (Point2d x (y + 1) : xs, query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll
