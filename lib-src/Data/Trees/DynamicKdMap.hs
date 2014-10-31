{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.DynamicKdMap
       ( DkdMap
       , PointAsListFn
       , SquaredDistanceFn
       , emptyDkdMap
       , singleton
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , insert
       , insertPair
       , null
       , size
       , assocs
       , keys
       , values
       , batchInsert
       , runTests
       ) where

import Prelude hiding (null)

import Control.Applicative
import Data.Bits
import Data.Foldable
import Data.Function
import Data.List as L hiding (insert, null)
import qualified Data.List (null)
import Data.Traversable

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Test.QuickCheck hiding ((.&.))

import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (PointAsListFn, SquaredDistanceFn, pointAsList2d, distSqr2d, Point2d (..))

data DkdMap k v = DkdMap
                  { _trees       :: [KDM.KdMap k v]
                  , _pointAsList :: PointAsListFn k
                  , _distSqr     :: SquaredDistanceFn k
                  , _numNodes    :: Int
                  } deriving Generic
instance (NFData k, NFData v) => NFData (DkdMap k v) where rnf = genericRnf

instance Functor (DkdMap k) where
  fmap f dkdMap = dkdMap { _trees = map (fmap f) $ _trees dkdMap }

foldrDkdMap :: ((k, v) -> a -> a) -> a -> DkdMap k v -> a
foldrDkdMap f z dkdMap = L.foldr (flip $ KDM.foldrKdMap f) z $ _trees dkdMap

instance Foldable (DkdMap k) where
  foldr f = foldrDkdMap (f . snd)

instance Traversable (DkdMap k) where
  traverse f (DkdMap t p d n) =
    DkdMap <$> traverse (traverse f) t <*> pure p <*> pure d <*> pure n

emptyDkdMap :: PointAsListFn k -> SquaredDistanceFn k -> DkdMap k v
emptyDkdMap p2l d2 = DkdMap [] p2l d2 0

null :: DkdMap k v -> Bool
null (DkdMap [] _ _ _) = True
null _ = False

singleton :: PointAsListFn k -> SquaredDistanceFn k -> (k, v) -> DkdMap k v
singleton p2l d2 (k, v) =
  DkdMap [KDM.buildKdMapWithDistFn p2l d2 [(k, v)]] p2l d2 1

nearestNeighbor :: DkdMap k v -> k -> (k, v)
nearestNeighbor (DkdMap ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DkdMap."
      else L.minimumBy (compare `on` (d2 query . fst)) nearests

insert :: DkdMap k v -> k -> v -> DkdMap k v
insert (DkdMap trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistFn p2l d2  $ (k, v) : L.concatMap KDM.assocs ones
  in  DkdMap (newTree : theRest) p2l d2 $ n + 1

insertPair :: DkdMap k v -> (k, v) -> DkdMap k v
insertPair t = uncurry (insert t)

kNearestNeighbors :: DkdMap k v -> Int -> k -> [(k, v)]
kNearestNeighbors (DkdMap trees _ d2 _) k query =
  let neighborSets = map (\t -> KDM.kNearestNeighbors t k query) trees
  in  take k $ L.foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = d2 query $ fst x
              distY = d2 query $ fst y

nearNeighbors :: DkdMap k v -> Double -> k -> [(k, v)]
nearNeighbors (DkdMap trees _ _ _) radius query =
  L.concatMap (\t -> KDM.nearNeighbors t radius query) trees

size :: DkdMap k v -> Int
size (DkdMap _ _ _ n) = n

assocs :: DkdMap k v -> [(k, v)]
assocs (DkdMap trees _ _ _) = L.concatMap KDM.assocs trees

keys :: DkdMap k v -> [k]
keys = map fst . assocs

values :: DkdMap k v -> [v]
values = map snd . assocs

-- TODO: This can be made far more efficient by batch-creating the
-- individual KdMaps before placing them into the DkdMap
batchInsert :: DkdMap k v -> [(k, v)] -> DkdMap k v
batchInsert =  L.foldl' insertPair

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN (DkdMap ts _ _ n) = length ts == popCount n
  in  L.all lengthIsLogN $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 (DkdMap ts _ _ _) = L.all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  L.all sizesPowerOf2 $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: PointAsListFn k -> SquaredDistanceFn k -> [k] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, DkdMap ts _ _ n) = n == num && n == L.sum (map (length . KDM.assocs) ts)
  in  L.all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: Eq k => PointAsListFn k -> SquaredDistanceFn k -> ([k], k) -> Bool
checkNearestEqualToBatch p2l d2 (ps, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDkdMap p2l d2) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch pointAsList2d distSqr2d (xs, query)

checkKNearestEqualToBatch :: Eq k => PointAsListFn k -> SquaredDistanceFn k -> ([k], Int, k) -> Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDkdMap p2l d2) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: KDM.Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch pointAsList2d distSqr2d (xs, k, query)

checkNearEqualToBatch :: Ord k => PointAsListFn k -> SquaredDistanceFn k -> ([k], Double, k) -> Bool
checkNearEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDkdMap p2l d2) $ testElements ps
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
