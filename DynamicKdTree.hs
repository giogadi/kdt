{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

-- TODO: Implement range find?
module Data.Trees.DynamicKdTree
       ( DkdTree
       , KDT.EuclideanSpace (..)
       , KDT.mk2DEuclideanSpace
       , emptyDkdTree
       , singleton
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , insert
       , insertPair
       , size
       , toList
       , batchInsert
       , runTests
       ) where

import Data.Bits
import Data.Function
import Data.List hiding (insert)

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Test.QuickCheck hiding ((.&.))

import qualified Data.Trees.KdTree as KDT

data DkdTree p d = DkdTree
                  { _trees    :: [KDT.KdTree p d]
                  , _space    :: KDT.EuclideanSpace p
                  , _numNodes :: Int
                  } deriving Generic
instance (NFData p, NFData d) => NFData (DkdTree p d) where rnf = genericRnf

-- TODO remove this
emptyDkdTree :: KDT.EuclideanSpace p -> DkdTree p d
emptyDkdTree s = DkdTree [] s 0

singleton :: KDT.EuclideanSpace p -> (p, d) -> DkdTree p d
singleton s (p, d) = DkdTree [KDT.buildKdTree s [(p, d)]] s 1

nearestNeighbor :: DkdTree p d -> p -> (p, d)
nearestNeighbor (DkdTree ts s _) query =
  let nearests = map (flip KDT.nearestNeighbor query) ts
  in  if   null nearests
      then error "Called nearestNeighbor on empty DkdTree."
      else minimumBy (compare `on` (KDT._dist2 s query . fst)) nearests

insert :: DkdTree p d -> p -> d -> DkdTree p d
insert (DkdTree trees s n) p d =
  let bitList = map (((.&.) 1) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDT.buildKdTree s $ (p, d) : concatMap KDT.toList ones
  in  DkdTree (newTree : theRest) s $ n + 1

insertPair :: DkdTree p d -> (p, d) -> DkdTree p d
insertPair t = uncurry (insert t)

kNearestNeighbors :: DkdTree p d -> Int -> p -> [(p, d)]
kNearestNeighbors (DkdTree trees s _) k query =
  let neighborSets = map (\t -> KDT.kNearestNeighbors t k query) trees
  in  take k $ foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = (KDT._dist2 s) query $ fst x
              distY = (KDT._dist2 s) query $ fst y

nearNeighbors :: DkdTree p d -> Double -> p -> [(p, d)]
nearNeighbors (DkdTree trees _ _) radius query =
  concatMap (\t -> KDT.nearNeighbors t radius query) trees

size :: DkdTree p d -> Int
size (DkdTree _ _ n) = n

toList :: DkdTree p d -> [(p, d)]
toList (DkdTree trees _ _) = concatMap KDT.toList trees

batchInsert :: DkdTree p d -> [(p, d)] -> DkdTree p d
batchInsert t =  foldl' insertPair t

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: KDT.EuclideanSpace p -> [p] -> Bool
checkLogNTrees s ps = let lengthIsLogN (DkdTree ts _ n) = length ts == popCount n
                      in  all lengthIsLogN $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_logNTrees :: [KDT.Point2d] -> Bool
prop_logNTrees = checkLogNTrees KDT.mk2DEuclideanSpace

checkTreeSizesPowerOf2 :: KDT.EuclideanSpace p -> [p] -> Bool
checkTreeSizesPowerOf2 s ps =
  let sizesPowerOf2 (DkdTree ts _ _) = all (== 1) $ map (popCount . length . KDT.toList) ts
  in  all sizesPowerOf2 $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_treeSizesPowerOf2 :: [KDT.Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 KDT.mk2DEuclideanSpace

checkNumElements :: KDT.EuclideanSpace p -> [p] -> Bool
checkNumElements s ps =
  let numsMatch (num, DkdTree ts _ n) = n == num && n == (sum $ map (length . KDT.toList) ts)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdTree s) $ testElements ps

prop_validNumElements :: [KDT.Point2d] -> Bool
prop_validNumElements = checkNumElements KDT.mk2DEuclideanSpace

checkNearestEqualToBatch :: Eq p => KDT.EuclideanSpace p -> ([p], p) -> Bool
checkNearestEqualToBatch s (ps, query) =
  let kdt = KDT.buildKdTree s $ testElements ps
      kdtAnswer = KDT.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: KDT.Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch KDT.mk2DEuclideanSpace (xs, query)

checkKNearestEqualToBatch :: Eq p => KDT.EuclideanSpace p -> ([p], Int, p) -> Bool
checkKNearestEqualToBatch s (ps, k, query) =
  let kdt = KDT.buildKdTree s $ testElements ps
      kdtAnswer = KDT.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: KDT.Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch KDT.mk2DEuclideanSpace (xs, k, query)

checkNearEqualToBatch :: Ord p => KDT.EuclideanSpace p -> ([p], Double, p) -> Bool
checkNearEqualToBatch s (ps, radius, query) =
  let kdt = KDT.buildKdTree s $ testElements ps
      kdtAnswer = KDT.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDkdTree s) $ testElements ps
      dkdtAnswer = nearNeighbors dkdt radius query
  in  sort dkdtAnswer == sort kdtAnswer

prop_checkNearEqualToBatch :: KDT.Point2d -> Property
prop_checkNearEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkNearEqualToBatch KDT.mk2DEuclideanSpace (xs, radius, query)

-- Run all tests
return []
runTests :: IO Bool
runTests =  $quickCheckAll
