{-# LANGUAGE DeriveGeneric #-}

-- TODO: Implement range find?
module Data.Trees.DynamicKdTree
       ( DkdTree
       , KDT.EuclideanSpace (..)
       , KDT.mk2DEuclideanSpace
       , emptyDkdTree
       , singleton
       , nearestNeighbor
       , kNearestNeighbors
       , insert
       , insertPair
       , size
       , toList
       , batchInsert
       -- Begin Tests
       , checkLogNTrees
       , checkTreeSizesPowerOf2
       , checkNumElements
       , checkNearestEqualToBatch
       , checkKNearestEqualToBatch
       , checkEqualToLinear
       , module Test.QuickCheck
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

-- TODO: separate (p, d) into distinct arguments
insert :: DkdTree p d -> p -> d -> DkdTree p d
insert (DkdTree trees s n) p d =
  let bitList = map (((.&.) 1) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDT.buildKdTree s $ (p, d) : concatMap KDT.toList ones
  in  DkdTree (newTree : theRest) s $ n + 1

insertPair :: DkdTree p d -> (p, d) -> DkdTree p d
insertPair t = uncurry (insert t)

kNearestNeighbors :: Eq p => DkdTree p d -> Int -> p -> [(p, d)]
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
testElements ps = zip ps [1..]

checkLogNTrees :: KDT.EuclideanSpace p -> [p] -> Bool
checkLogNTrees s ps = let lengthIsLogN (DkdTree ts _ n) = length ts == popCount n
                      in  all lengthIsLogN $ scanl insertPair (emptyDkdTree s) $ testElements ps

checkTreeSizesPowerOf2 :: KDT.EuclideanSpace p -> [p] -> Bool
checkTreeSizesPowerOf2 s ps =
  let sizesPowerOf2 (DkdTree ts _ _) = all (== 1) $ map (popCount . length . KDT.toList) ts
  in  all sizesPowerOf2 $ scanl insertPair (emptyDkdTree s) $ testElements ps

checkNumElements :: KDT.EuclideanSpace p -> [p] -> Bool
checkNumElements s ps =
  let numsMatch (num, DkdTree ts _ n) = n == num && n == (sum $ map (length . KDT.toList) ts)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdTree s) $ testElements ps

checkNearestEqualToBatch :: Eq p => KDT.EuclideanSpace p -> ([p], p) -> Bool
checkNearestEqualToBatch _ ([], _) = True
checkNearestEqualToBatch s (ps, query) =
  let kdt = KDT.buildKdTree s $ testElements ps
      kdtAnswer = KDT.nearestNeighbor kdt query
      dkdt = foldl' insertPair (emptyDkdTree s) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

checkKNearestEqualToBatch :: Eq p => KDT.EuclideanSpace p -> ([p], p) -> Bool
checkKNearestEqualToBatch _ ([], _) = True
checkKNearestEqualToBatch s (ps, query) =
  let k = 10 -- TODO make this quick-checked and bounded by something reasonable
      kdt = KDT.buildKdTree s $ testElements ps
      kdtAnswer = KDT.kNearestNeighbors kdt k query
      dkdt = foldl' insertPair (emptyDkdTree s) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

nearestNeighborLinear :: KDT.EuclideanSpace p -> [(p, d)] -> p -> (p, d)
nearestNeighborLinear s xs query =
  minimumBy (compare `on` (KDT._dist2 s query . fst)) xs

checkEqualToLinear :: Eq p => KDT.EuclideanSpace p -> ([p], p) -> Bool
checkEqualToLinear _ ([], _) = True
checkEqualToLinear s (ps, query) =
  let (testElem1 : testElems) = testElements ps
      linears = scanl (\xs x -> xs ++ [x]) [testElem1] $ testElems
      dkdts   = scanl insertPair (singleton s testElem1) $ testElems
  in  map (\xs -> nearestNeighborLinear s xs query) linears == map (`nearestNeighbor` query) dkdts
