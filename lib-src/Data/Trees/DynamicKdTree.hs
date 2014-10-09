{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

-- TODO: Implement range find?
module Data.Trees.DynamicKdTree
       ( DkdTree
       , KdSpace (..)
       , mk2DEuclideanSpace
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
import Data.Trees.KdMap (KdSpace, mk2DEuclideanSpace)

data DkdTree k v = DkdTree
                  { _trees    :: [KDM.KdMap k v]
                  , _space    :: KDM.KdSpace k
                  , _numNodes :: Int
                  } deriving Generic
instance (NFData k, NFData v) => NFData (DkdTree k v) where rnf = genericRnf

-- TODO remove this
emptyDkdTree :: KDM.KdSpace k -> DkdTree k v
emptyDkdTree s = DkdTree [] s 0

null :: DkdTree k v -> Bool
null (DkdTree [] _ _) = True
null _ = False

singleton :: KDM.KdSpace k -> (k, v) -> DkdTree k v
singleton s (p, d) = DkdTree [KDM.buildKdMap s [(p, d)]] s 1

nearestNeighbor :: DkdTree k v -> k -> (k, v)
nearestNeighbor (DkdTree ts s _) query =
  let nearests = map (flip KDM.nearestNeighbor query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DkdTree."
      else minimumBy (compare `on` (KDM._distSqr s query . fst)) nearests

insert :: DkdTree k v -> k -> v -> DkdTree k v
insert (DkdTree trees s n) p d =
  let bitList = map (((.&.) 1) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMap s $ (p, d) : concatMap KDM.assocs ones
  in  DkdTree (newTree : theRest) s $ n + 1

insertPair :: DkdTree k v -> (k, v) -> DkdTree k v
insertPair t = uncurry (insert t)

kNearestNeighbors :: DkdTree k v -> Int -> k -> [(k, v)]
kNearestNeighbors (DkdTree trees s _) k query =
  let neighborSets = map (\t -> KDM.kNearestNeighbors t k query) trees
  in  take k $ foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = (KDM._distSqr s) query $ fst x
              distY = (KDM._distSqr s) query $ fst y

nearNeighbors :: DkdTree k v -> Double -> k -> [(k, v)]
nearNeighbors (DkdTree trees _ _) radius query =
  concatMap (\t -> KDM.nearNeighbors t radius query) trees

size :: DkdTree k v -> Int
size (DkdTree _ _ n) = n

toList :: DkdTree k v -> [(k, v)]
toList (DkdTree trees _ _) = concatMap KDM.assocs trees

batchInsert :: DkdTree k v -> [(k, v)] -> DkdTree k v
batchInsert t =  foldl' insertPair t

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: KDM.KdSpace k -> [k] -> Bool
checkLogNTrees s ps = let lengthIsLogN (DkdTree ts _ n) = length ts == popCount n
                      in  all lengthIsLogN $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_logNTrees :: [KDM.Point2d] -> Bool
prop_logNTrees = checkLogNTrees KDM.mk2DEuclideanSpace

checkTreeSizesPowerOf2 :: KDM.KdSpace k -> [k] -> Bool
checkTreeSizesPowerOf2 s ps =
  let sizesPowerOf2 (DkdTree ts _ _) = all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  all sizesPowerOf2 $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_treeSizesPowerOf2 :: [KDM.Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 KDM.mk2DEuclideanSpace

checkNumElements :: KDM.KdSpace k -> [k] -> Bool
checkNumElements s ps =
  let numsMatch (num, DkdTree ts _ n) = n == num && n == (sum $ map (length . KDM.assocs) ts)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_validNumElements :: [KDM.Point2d] -> Bool
prop_validNumElements = checkNumElements KDM.mk2DEuclideanSpace

checkNearestEqualToBatch :: Eq k => KDM.KdSpace k -> ([k], k) -> Bool
checkNearestEqualToBatch s (ps, query) =
  let kdt = KDM.buildKdMap s $ testElements ps
      kdtAnswer = KDM.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: KDM.Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch KDM.mk2DEuclideanSpace (xs, query)

checkKNearestEqualToBatch :: Eq k => KDM.KdSpace k -> ([k], Int, k) -> Bool
checkKNearestEqualToBatch s (ps, k, query) =
  let kdt = KDM.buildKdMap s $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: KDM.Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch KDM.mk2DEuclideanSpace (xs, k, query)

checkNearEqualToBatch :: Ord k => KDM.KdSpace k -> ([k], Double, k) -> Bool
checkNearEqualToBatch s (ps, radius, query) =
  let kdt = KDM.buildKdMap s $ testElements ps
      kdtAnswer = KDM.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = nearNeighbors dkdt radius query
  in  sort dkdtAnswer == sort kdtAnswer

prop_checkNearEqualToBatch :: KDM.Point2d -> Property
prop_checkNearEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkNearEqualToBatch KDM.mk2DEuclideanSpace (xs, radius, query)

-- Run all tests
return []
runTests :: IO Bool
runTests =  $quickCheckAll
