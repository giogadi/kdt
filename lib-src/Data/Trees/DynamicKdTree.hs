{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

-- TODO: Implement range find?
module Data.Trees.DynamicKdTree
       ( DkdTree
       , PointAsListFn
       , SquaredDistanceFn
       , emptyDkdTree
       , singleton
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , insert
       , insertPair
       , null
       , size
       , toList
       , batchInsert
       , runTests
       ) where

import Prelude hiding (null)

import Data.Bits
import Data.Function
import Data.List hiding (insert, null)
import qualified Data.List (null)

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Test.QuickCheck hiding ((.&.))

import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (PointAsListFn, SquaredDistanceFn, pointAsList2d, distSqr2d, Point2d (..))

data DkdTree k v = DkdTree
                  { _trees       :: [KDM.KdMap k v]
                  , _pointAsList :: PointAsListFn k
                  , _distSqr     :: SquaredDistanceFn k
                  , _numNodes    :: Int
                  } deriving Generic
instance (NFData k, NFData v) => NFData (DkdTree k v) where rnf = genericRnf

-- TODO remove this
emptyDkdTree :: PointAsListFn k -> SquaredDistanceFn k -> DkdTree k v
emptyDkdTree p2l d2 = DkdTree [] p2l d2 0

null :: DkdTree k v -> Bool
null (DkdTree [] _ _ _) = True
null _ = False

singleton :: PointAsListFn k -> SquaredDistanceFn k -> (k, v) -> DkdTree k v
singleton p2l d2 (k, v) =
  DkdTree [KDM.buildKdMapWithDistSqrFn p2l d2 [(k, v)]] p2l d2 1

nearestNeighbor :: DkdTree k v -> k -> (k, v)
nearestNeighbor (DkdTree ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DkdTree."
      else minimumBy (compare `on` (d2 query . fst)) nearests

insert :: DkdTree k v -> k -> v -> DkdTree k v
insert (DkdTree trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistSqrFn p2l d2  $ (k, v) : concatMap KDM.assocs ones
  in  DkdTree (newTree : theRest) p2l d2 $ n + 1

insertPair :: DkdTree k v -> (k, v) -> DkdTree k v
insertPair t = uncurry (insert t)

kNearestNeighbors :: DkdTree k v -> Int -> k -> [(k, v)]
kNearestNeighbors (DkdTree trees _ d2 _) k query =
  let neighborSets = map (\t -> KDM.kNearestNeighbors t k query) trees
  in  take k $ foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = d2 query $ fst x
              distY = d2 query $ fst y

nearNeighbors :: DkdTree k v -> Double -> k -> [(k, v)]
nearNeighbors (DkdTree trees _ _ _) radius query =
  concatMap (\t -> KDM.nearNeighbors t radius query) trees

size :: DkdTree k v -> Int
size (DkdTree _ _ _ n) = n

toList :: DkdTree k v -> [(k, v)]
toList (DkdTree trees _ _ _) = concatMap KDM.assocs trees

batchInsert :: DkdTree k v -> [(k, v)] -> DkdTree k v
batchInsert =  foldl' insertPair

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN (DkdTree ts _ _ n) = length ts == popCount n
  in  all lengthIsLogN $ scanl insertPair (emptyDkdTree p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 (DkdTree ts _ _ _) = all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  all sizesPowerOf2 $ scanl insertPair (emptyDkdTree p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, DkdTree ts _ _ n) = n == num && n == sum (map (length . KDM.assocs) ts)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdTree p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: Eq k => PointAsListFn k -> SquaredDistanceFn k -> ([k], k) -> Bool
checkNearestEqualToBatch p2l d2 (ps, query) =
  let kdt = KDM.buildKdMapWithDistSqrFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDkdTree p2l d2) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch pointAsList2d distSqr2d (xs, query)

checkKNearestEqualToBatch :: Eq k => PointAsListFn k -> SquaredDistanceFn k -> ([k], Int, k) -> Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildKdMapWithDistSqrFn p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDkdTree p2l d2) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: KDM.Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch pointAsList2d distSqr2d (xs, k, query)

checkNearEqualToBatch :: Ord k => PointAsListFn k -> SquaredDistanceFn k -> ([k], Double, k) -> Bool
checkNearEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildKdMapWithDistSqrFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDkdTree p2l d2) $ testElements ps
      dkdtAnswer = nearNeighbors dkdt radius query
  in  sort dkdtAnswer == sort kdtAnswer

prop_checkNearEqualToBatch :: Point2d -> Property
prop_checkNearEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkNearEqualToBatch pointAsList2d distSqr2d (xs, radius, query)

-- Run all tests
return []
runTests :: IO Bool
runTests =  $quickCheckAll
