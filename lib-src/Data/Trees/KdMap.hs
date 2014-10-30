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
                 { _pointAsList  :: k -> [Double]
                 , _distSqr :: k -> k -> Double
                 } deriving Generic
instance NFData k => NFData (KdSpace k) where rnf = genericRnf

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

data KdMap k v = KdMap (KdSpace k) (TreeNode k v) Int deriving Generic
instance (NFData k, NFData v) => NFData (KdMap k v) where rnf = genericRnf

instance Functor (KdMap k) where
  fmap f (KdMap s t n) = KdMap s (mapTreeNode f t) n

instance Foldable (KdMap k) where
  foldr f z (KdMap _ t _) = go f z t
    where go f' z' (TreeNode Nothing (_, v) _ Nothing) = f' v z'
          go f' z' (TreeNode (Just l) (_, v) _ Nothing) = go f' (f' v z') l
          go f' z' (TreeNode Nothing (_, v) _ (Just r)) = f' v (go f' z' r)
          go f' z' (TreeNode (Just l) (_, v) _ (Just r)) = go f' (f' v (go f' z' r)) l

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
buildKdMap s points =
  let axisValsPointsPairs = zip (map (cycle . _pointAsList s . fst) points) points
  in  KdMap s (buildTreeInternal axisValsPointsPairs) $ length points
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

assocsInternal :: TreeNode k v -> [(k, v)]
assocsInternal t = go t []
  where go (TreeNode l p _ r) =
          maybe id go l . (p :) . maybe id go r

assocs :: KdMap k v -> [(k, v)]
assocs (KdMap _ t _) = assocsInternal t

keys :: KdMap k v -> [k]
keys = map fst . assocs

values :: KdMap k v -> [v]
values = map snd . assocs

nearestNeighbor :: KdMap k v -> k -> (k, v)
nearestNeighbor (KdMap s t@(TreeNode _ root _ _) _) query =
  -- This is an ugly way to kickstart the function but it's faster
  -- than using a Maybe.
  fst $ go (root, 1 / 0 :: Double) (cycle $ _pointAsList s query) t
  where
    go _ [] _ = error "nearestNeighbor.go: no empty lists allowed!"
    go bestSoFar (queryAxisValue : qvs) (TreeNode maybeLeft (curr_p, curr_d) curr_v maybeRight) =
      let better (x1, dist1) (x2, dist2) = if dist1 < dist2
                                           then (x1, dist1)
                                           else (x2, dist2)
          currDist       = _distSqr s query curr_p
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
nearNeighbors (KdMap s t _) radius query = go (cycle $ _pointAsList s query) t
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
          currentNear = if _distSqr s p query <= radius * radius
                        then [(p, d)]
                        else []
      in  onsideNear ++ currentNear ++ offsideNear

kNearestNeighbors :: KdMap k v -> Int -> k -> [(k, v)]
kNearestNeighbors (KdMap s t _) k query =
  reverse $ map snd $ Q.toList $ go (cycle $ _pointAsList s query) Q.empty t
  where
    -- go :: [Double] -> Q.MaxPQueue Double (p, d) -> TreeNode p d -> KQueue p d
    go [] _ _ = error "kNearestNeighbors.go: no empty lists allowed!"
    go (queryAxisValue : qvs) q (TreeNode maybeLeft (p, d) xAxisValue maybeRight) =
      let insertBounded queue dist x
            | Q.size queue < k = Q.insert dist x queue
            | otherwise = if dist < fst (Q.findMax queue)
                          then Q.deleteMax $ Q.insert dist x queue
                          else queue
          q' = insertBounded q (_distSqr s p query) (p, d)
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
size (KdMap _ _ n) = n

data Point2d = Point2d Double Double deriving (Show, Eq, Ord, Generic)
instance NFData Point2d where rnf = genericRnf

mk2DEuclideanSpace :: KdSpace Point2d
mk2DEuclideanSpace = KdSpace
                     { _pointAsList  = pointAsList
                     , _distSqr = dist
                     }
 where pointAsList (Point2d x y) = [x, y]
       dist (Point2d x1 y1) (Point2d x2 y2) = let dx = x2 - x1
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
testElements ps = zip ps $ [0 ..]

isTreeValid :: KdSpace k -> Int -> TreeNode k v -> Bool
isTreeValid s axis (TreeNode l (p, _) xAxisVal r) =
  let nodeAxisVal (TreeNode _ (p', _) _ _) = _pointAsList s p' !! axis
      nextAxis = (axis + 1) `mod` length (_pointAsList s p)
      -- TODO: Shouldn't it be < and >=?
      leftChildValid = maybe True ((<= xAxisVal) . nodeAxisVal) l
      rightChildValid = maybe True ((> xAxisVal) . nodeAxisVal) r
      leftSubtreeValid = maybe True (isTreeValid s nextAxis) l
      rightSubtreeValid = maybe True (isTreeValid s nextAxis) r
  in  leftChildValid && rightChildValid && leftSubtreeValid && rightSubtreeValid

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
