{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.DynKdMap
       ( DynKdMap
       , PointAsListFn
       , SquaredDistanceFn
       , emptyDynKdMap
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
       , foldrDynKdMap
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

import Data.Point2d
import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (PointAsListFn, SquaredDistanceFn)

data DynKdMap a k v = DynKdMap
                      { _trees       :: [KDM.KdMap a k v]
                      , _pointAsList :: PointAsListFn a k
                      , _distSqr     :: SquaredDistanceFn a k
                      , _numNodes    :: Int
                      } deriving Generic
instance (NFData k, NFData a, NFData v) => NFData (DynKdMap a k v) where rnf = genericRnf

instance Functor (DynKdMap a k) where
  fmap f dkdMap = dkdMap { _trees = map (fmap f) $ _trees dkdMap }

foldrDynKdMap :: ((k, v) -> b -> b) -> b -> DynKdMap a k v -> b
foldrDynKdMap f z dkdMap = L.foldr (flip $ KDM.foldrKdMap f) z $ _trees dkdMap

instance Foldable (DynKdMap a k) where
  foldr f = foldrDynKdMap (f . snd)

instance Traversable (DynKdMap a k) where
  traverse f (DynKdMap t p d n) =
    DynKdMap <$> traverse (traverse f) t <*> pure p <*> pure d <*> pure n

emptyDynKdMap :: PointAsListFn a k -> SquaredDistanceFn a k -> DynKdMap a k v
emptyDynKdMap p2l d2 = DynKdMap [] p2l d2 0

null :: DynKdMap a k v -> Bool
null (DynKdMap [] _ _ _) = True
null _ = False

singleton :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> (k, v) -> DynKdMap a k v
singleton p2l d2 (k, v) =
  DynKdMap [KDM.buildKdMapWithDistFn p2l d2 [(k, v)]] p2l d2 1

nearestNeighbor :: Real a => DynKdMap a k v -> k -> (k, v)
nearestNeighbor (DynKdMap ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DynKdMap."
      else L.minimumBy (compare `on` (d2 query . fst)) nearests

insert :: Real a => DynKdMap a k v -> k -> v -> DynKdMap a k v
insert (DynKdMap trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistFn p2l d2  $ (k, v) : L.concatMap KDM.assocs ones
  in  DynKdMap (newTree : theRest) p2l d2 $ n + 1

insertPair :: Real a => DynKdMap a k v -> (k, v) -> DynKdMap a k v
insertPair t = uncurry (insert t)

kNearestNeighbors :: Real a => DynKdMap a k v -> Int -> k -> [(k, v)]
kNearestNeighbors (DynKdMap trees _ d2 _) k query =
  let neighborSets = map (\t -> KDM.kNearestNeighbors t k query) trees
  in  take k $ L.foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = d2 query $ fst x
              distY = d2 query $ fst y

nearNeighbors :: Real a => DynKdMap a k v -> a -> k -> [(k, v)]
nearNeighbors (DynKdMap trees _ _ _) radius query =
  L.concatMap (\t -> KDM.nearNeighbors t radius query) trees

size :: DynKdMap a k v -> Int
size (DynKdMap _ _ _ n) = n

assocs :: DynKdMap a k v -> [(k, v)]
assocs (DynKdMap trees _ _ _) = L.concatMap KDM.assocs trees

keys :: DynKdMap a k v -> [k]
keys = map fst . assocs

values :: DynKdMap a k v -> [v]
values = map snd . assocs

-- TODO: This can be made far more efficient by batch-creating the
-- individual KdMaps before placing them into the DynKdMap
batchInsert :: Real a => DynKdMap a k v -> [(k, v)] -> DynKdMap a k v
batchInsert =  L.foldl' insertPair

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> [k] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN (DynKdMap ts _ _ n) = length ts == popCount n
  in  L.all lengthIsLogN $ scanl insertPair (emptyDynKdMap p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: Real a => PointAsListFn a k ->
                                    SquaredDistanceFn a k ->
                                    [k] ->
                                    Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 (DynKdMap ts _ _ _) = L.all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  L.all sizesPowerOf2 $ scanl insertPair (emptyDynKdMap p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> [k] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, DynKdMap ts _ _ n) = n == num && n == L.sum (map (length . KDM.assocs) ts)
  in  L.all numsMatch $ zip [0..] $ scanl insertPair (emptyDynKdMap p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: (Eq k, Real a) => PointAsListFn a k ->
                                              SquaredDistanceFn a k ->
                                              ([k], k) ->
                                              Bool
checkNearestEqualToBatch p2l d2 (ps, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDynKdMap p2l d2) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch pointAsList2d distSqr2d (xs, query)

checkKNearestEqualToBatch :: (Eq k, Real a) => PointAsListFn a k ->
                                               SquaredDistanceFn a k ->
                                               ([k], Int, k) ->
                                               Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDynKdMap p2l d2) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch pointAsList2d distSqr2d (xs, k, query)

checkNearEqualToBatch :: (Ord k, Real a) => PointAsListFn a k ->
                                            SquaredDistanceFn a k ->
                                            ([k], a, k) ->
                                            Bool
checkNearEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDynKdMap p2l d2) $ testElements ps
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