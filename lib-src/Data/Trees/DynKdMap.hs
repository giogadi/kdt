{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Trees.DynKdMap
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , DynKdMap
         -- ** Dynamic /k/-d map construction
       , emptyDynKdMap
       , singleton
       , emptyDynKdMapWithDistFn
       , singletonWithDistFn
         -- ** Insertion
       , insert
       , batchInsert
         -- ** Query
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , assocs
       , points
       , values
       , null
       , size
         -- ** Folds
       , foldrDynKdMap
         -- ** Utilities
       , defaultDistSqrFn
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
import Data.Trees.KdMap (PointAsListFn, SquaredDistanceFn, defaultDistSqrFn)

-- $usage
--
-- The 'DynKdMap' is a variant of 'Data.Trees.DynKdTree' where each
-- point in the tree is associated with some data. It is the dynamic
-- variant of 'Data.Trees.KdMap'.
--
-- Here's an example of interleaving point-value insertions and point
-- queries using 'DynKdMap', where points are 3D points and values are
-- 'String's:
--
-- @
-- >>> let dkdm = singleton point3dAsList ((Point3D 0.0 0.0 0.0), \"First\")
--
-- >>> let dkdm' = insert dkdm ((Point3D 1.0 1.0 1.0), \"Second\")
--
-- >>> nearestNeighbor dkdm' (Point3D 0.4 0.4 0.4)
-- (Point3D {x = 0.0, y = 0.0, z = 0.0}, \"First\")
--
-- >>> let dkdm'' = insert dkdm' ((Point3D 0.5 0.5 0.5), \"Third\")
--
-- >>> nearestNeighbor dkdm'' (Point3D 0.4 0.4 0.4)
-- (Point3D {x = 0.5, y = 0.5, z = 0.5}, \"Third\")
-- @

-- | A dynamic /k/-d tree structure that stores points of type @p@
-- with axis values of type @a@. Additionally, each point is
-- associated with a value of type @v@.
data DynKdMap a p v = DynKdMap
                      { _trees       :: [KDM.KdMap a p v]
                      , _pointAsList :: PointAsListFn a p
                      , _distSqr     :: SquaredDistanceFn a p
                      , _numNodes    :: Int
                      } deriving Generic
instance (NFData a, NFData p, NFData v) => NFData (DynKdMap a p v) where rnf = genericRnf

instance Functor (DynKdMap a p) where
  fmap f dkdMap = dkdMap { _trees = map (fmap f) $ _trees dkdMap }

-- | Performs a foldr over each point-value pair in the 'DynKdMap'.
foldrDynKdMap :: ((p, v) -> b -> b) -> b -> DynKdMap a p v -> b
foldrDynKdMap f z dkdMap = L.foldr (flip $ KDM.foldrKdMap f) z $ _trees dkdMap

instance Foldable (DynKdMap a p) where
  foldr f = foldrDynKdMap (f . snd)

instance Traversable (DynKdMap a p) where
  traverse f (DynKdMap t p d n) =
    DynKdMap <$> traverse (traverse f) t <*> pure p <*> pure d <*> pure n

-- | Generates an empty 'DynKdMap' with a user-specified distance function.
emptyDynKdMapWithDistFn :: PointAsListFn a p -> SquaredDistanceFn a p -> DynKdMap a p v
emptyDynKdMapWithDistFn p2l d2 = DynKdMap [] p2l d2 0

-- | Returns whether the 'DynKdMap' is empty.
null :: DynKdMap a p v -> Bool
null (DynKdMap [] _ _ _) = True
null _ = False

-- | Generates a 'DynKdMap' with a single point-value pair using a
-- user-specified distance function.
singletonWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> (p, v) -> DynKdMap a p v
singletonWithDistFn p2l d2 (k, v) =
  DynKdMap [KDM.buildKdMapWithDistFn p2l d2 [(k, v)]] p2l d2 1

-- | Generates an empty 'DynKdMap' with the default distance function.
emptyDynKdMap :: Real a => PointAsListFn a p -> DynKdMap a p v
emptyDynKdMap p2l = emptyDynKdMapWithDistFn p2l $ defaultDistSqrFn p2l

-- | Generates a 'DynKdMap' with a single point-value pair using the
-- default distance function.
singleton :: Real a => PointAsListFn a p -> (p, v) -> DynKdMap a p v
singleton p2l = singletonWithDistFn p2l $ defaultDistSqrFn p2l

-- | Adds a given point-value pair to a 'DynKdMap'.
--
-- Average time complexity per insert for /n/ inserts: /O(log^2(n))/.
insert :: Real a => DynKdMap a p v -> p -> v -> DynKdMap a p v
insert (DynKdMap trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistFn p2l d2  $ (k, v) : L.concatMap KDM.assocs ones
  in  DynKdMap (newTree : theRest) p2l d2 $ n + 1

-- | Given a 'DynKdMap' and a query point, returns the point-value pair in
-- the 'DynKdMap' with the point nearest to the query.
--
-- Average time complexity: /O(log^2(n))/.
nearestNeighbor :: Real a => DynKdMap a p v -> p -> (p, v)
nearestNeighbor (DynKdMap ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty DynKdMap."
      else L.minimumBy (compare `on` (d2 query . fst)) nearests

insertPair :: Real a => DynKdMap a p v -> (p, v) -> DynKdMap a p v
insertPair t = uncurry (insert t)

-- | Given a 'DynKdMap', a query point, and a number @k@, returns the
-- @k@ point-value pairs with the nearest points to the query.
--
-- TODO: time complexity.
kNearestNeighbors :: Real a => DynKdMap a p v -> Int -> p -> [(p, v)]
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

-- | Given a 'DynKdMap', a query point, and a radius, returns all
-- point-value pairs in the 'DynKdTree' with points within the given
-- radius of the query point.
--
-- TODO: time complexity.
nearNeighbors :: Real a => DynKdMap a p v -> a -> p -> [(p, v)]
nearNeighbors (DynKdMap trees _ _ _) radius query =
  L.concatMap (\t -> KDM.nearNeighbors t radius query) trees

-- | Returns the number of elements in the 'DynKdMap'.
--
-- Time complexity: /O(1)/
size :: DynKdMap a p v -> Int
size (DynKdMap _ _ _ n) = n

-- | Returns a list of all the point-value pairs in the 'DynKdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
assocs :: DynKdMap a p v -> [(p, v)]
assocs (DynKdMap trees _ _ _) = L.concatMap KDM.assocs trees

-- | Returns all points in the 'DynKdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
points :: DynKdMap a p v -> [p]
points = map fst . assocs

-- | Returns all values in the 'DynKdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
values :: DynKdMap a p v -> [v]
values = map snd . assocs

-- | Inserts a list of point-value pairs into the 'DynKdMap'.
batchInsert :: Real a => DynKdMap a p v -> [(p, v)] -> DynKdMap a p v
-- TODO: This can be made far more efficient by batch-creating the
-- individual KdMaps before placing them into the DynKdMap
batchInsert =  L.foldl' insertPair

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN (DynKdMap ts _ _ n) = length ts == popCount n
  in  L.all lengthIsLogN $ scanl insertPair (emptyDynKdMapWithDistFn p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: Real a => PointAsListFn a p ->
                                    SquaredDistanceFn a p ->
                                    [p] ->
                                    Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 (DynKdMap ts _ _ _) = L.all (== 1) $ map (popCount . length . KDM.assocs) ts
  in  L.all sizesPowerOf2 $ scanl insertPair (emptyDynKdMapWithDistFn p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, DynKdMap ts _ _ n) = n == num && n == L.sum (map (length . KDM.assocs) ts)
  in  L.all numsMatch $ zip [0..] $ scanl insertPair (emptyDynKdMapWithDistFn p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: (Eq p, Real a) => PointAsListFn a p ->
                                              SquaredDistanceFn a p ->
                                              ([p], p) ->
                                              Bool
checkNearestEqualToBatch p2l d2 (ps, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearestNeighbor kdt query
      dkdt = batchInsert (emptyDynKdMapWithDistFn p2l d2) $ testElements ps
      dkdtAnswer = nearestNeighbor dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch pointAsList2d distSqr2d (xs, query)

checkKNearestEqualToBatch :: (Eq p, Real a) => PointAsListFn a p ->
                                               SquaredDistanceFn a p ->
                                               ([p], Int, p) ->
                                               Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearestNeighbors kdt k query
      dkdt = batchInsert (emptyDynKdMapWithDistFn p2l d2) $ testElements ps
      dkdtAnswer = kNearestNeighbors dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch pointAsList2d distSqr2d (xs, k, query)

checkNearEqualToBatch :: (Ord p, Real a) => PointAsListFn a p ->
                                            SquaredDistanceFn a p ->
                                            ([p], a, p) ->
                                            Bool
checkNearEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildKdMapWithDistFn p2l d2 $ testElements ps
      kdtAnswer = KDM.nearNeighbors kdt radius query
      dkdt = batchInsert (emptyDynKdMapWithDistFn p2l d2) $ testElements ps
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
