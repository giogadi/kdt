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
       , foldrDkdMap
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

data DkdMap a k v = DkdMap
                    { _trees       :: [KDM.KdMap a k v]
                    , _pointAsList :: PointAsListFn a k
                    , _distSqr     :: SquaredDistanceFn a k
                    , _numNodes    :: Int
                    } deriving Generic
instance (NFData k, NFData a, NFData v) => NFData (DkdMap a k v) where rnf = genericRnf

instance Functor (DkdMap a k) where
  fmap f dkdMap = dkdMap { _trees = map (fmap f) $ _trees dkdMap }

foldrDkdMap :: ((k, v) -> b -> b) -> b -> DkdMap a k v -> b
foldrDkdMap f z dkdMap = L.foldr (flip $ KDM.foldrKdMap f) z $ _trees dkdMap

instance Foldable (DkdMap a k) where
  foldr f = foldrDkdMap (f . snd)

instance Traversable (DkdMap a k) where
  traverse f (DkdMap t p d n) =
    DkdMap <$> traverse (traverse f) t <*> pure p <*> pure d <*> pure n

emptyDkdMap :: PointAsListFn a k -> SquaredDistanceFn a k -> DkdMap a k v
emptyDkdMap p2l d2 = DkdMap [] p2l d2 0

null :: DkdMap a k v -> Bool
null (DkdMap [] _ _ _) = True
null _ = False

singleton :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> (k, v) -> DkdMap a k v
singleton p2l d2 (k, v) =
  DkdMap [KDM.buildKdMapWithDistFn p2l d2 [(k, v)]] p2l d2 1

nearestNeighbor :: Real a => DkdMap a k v -> k -> (k, v)
nearestNeighbor (DkdMap ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DkdMap."
      else L.minimumBy (compare `on` (d2 query . fst)) nearests

insert :: Real a => DkdMap a k v -> k -> v -> DkdMap a k v
insert (DkdMap trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistFn p2l d2  $ (k, v) : L.concatMap KDM.assocs ones
  in  DkdMap (newTree : theRest) p2l d2 $ n + 1

insertPair :: Real a => DkdMap a k v -> (k, v) -> DkdMap a k v
insertPair t = uncurry (insert t)

kNearestNeighbors :: Real a => DkdMap a k v -> Int -> k -> [(k, v)]
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

nearNeighbors :: Real a => DkdMap a k v -> a -> k -> [(k, v)]
nearNeighbors (DkdMap trees _ _ _) radius query =
  L.concatMap (\t -> KDM.nearNeighbors t radius query) trees

size :: DkdMap a k v -> Int
size (DkdMap _ _ _ n) = n

assocs :: DkdMap a k v -> [(k, v)]
assocs (DkdMap trees _ _ _) = L.concatMap KDM.assocs trees

keys :: DkdMap a k v -> [k]
keys = map fst . assocs

values :: DkdMap a k v -> [v]
values = map snd . assocs

-- TODO: This can be made far more efficient by batch-creating the
-- individual KdMaps before placing them into the DkdMap
batchInsert :: Real a => DkdMap a k v -> [(k, v)] -> DkdMap a k v
batchInsert =  L.foldl' insertPair

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [k] -> [(k, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> [k] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN (DkdMap ts _ _ n) = length ts == popCount n
  in  L.all lengthIsLogN $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: Real a => PointAsListFn a k ->
                                    SquaredDistanceFn a k ->
                                    [k] ->
                                    Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 (DkdMap ts _ _ _) = L.all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  L.all sizesPowerOf2 $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: Real a => PointAsListFn a k -> SquaredDistanceFn a k -> [k] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, DkdMap ts _ _ n) = n == num && n == L.sum (map (length . KDM.assocs) ts)
  in  L.all numsMatch $ zip [0..] $ scanl insertPair (emptyDkdMap p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: (Eq k, Real a) => PointAsListFn a k ->
                                              SquaredDistanceFn a k ->
                                              ([k], k) ->
                                              Bool
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

checkKNearestEqualToBatch :: (Eq k, Real a) => PointAsListFn a k ->
                                               SquaredDistanceFn a k ->
                                               ([k], Int, k) ->
                                               Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDkdMap p2l d2) $ testElements ps
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
