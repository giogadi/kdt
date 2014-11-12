{-# LANGUAGE DeriveGeneric #-}

module Data.KdMap.Dynamic
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdMap
         -- ** Dynamic /k/-d map construction
       , emptyKdMap
       , singleton
       , emptyKdMapWithDistFn
       , singletonWithDistFn
         -- ** Insertion
       , insert
       , insertPair
       , batchInsert
         -- ** Query
       , nearestNeighbor
       , pointsInRadius
       , kNearestNeighbors
       , pointsInRange
       , assocs
       , points
       , values
       , null
       , size
         -- ** Folds
       , foldrKdMap
         -- ** Utilities
       , defaultDistSqrFn
         -- ** Internal (for testing)
       , subtreeSizes
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

import qualified Data.KdMap.Static as KDM
import Data.KdMap.Static (PointAsListFn, SquaredDistanceFn, defaultDistSqrFn)

-- $usage
--
-- The 'KdMap' is a variant of
-- @Data.KdTree.Dynamic.@'Data.KdTree.Dynamic.KdTree' where each point
-- in the tree is associated with some data. It is the dynamic variant
-- of @Data.KdMap.Static.@'Data.KdMap.Static.KdMap'.
--
-- Here's an example of interleaving point-value insertions and point
-- queries using 'KdMap', where points are 3D points and values are
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
data KdMap a p v = KdMap
                   { _trees       :: [KDM.KdMap a p v]
                   , _pointAsList :: PointAsListFn a p
                   , _distSqr     :: SquaredDistanceFn a p
                   , _numNodes    :: Int
                   } deriving Generic
instance (NFData a, NFData p, NFData v) => NFData (KdMap a p v) where rnf = genericRnf

instance Functor (KdMap a p) where
  fmap f dkdMap = dkdMap { _trees = map (fmap f) $ _trees dkdMap }

-- | Performs a foldr over each point-value pair in the 'KdMap'.
foldrKdMap :: ((p, v) -> b -> b) -> b -> KdMap a p v -> b
foldrKdMap f z dkdMap = L.foldr (flip $ KDM.foldrKdMap f) z $ _trees dkdMap

instance Foldable (KdMap a p) where
  foldr f = foldrKdMap (f . snd)

instance Traversable (KdMap a p) where
  traverse f (KdMap t p d n) =
    KdMap <$> traverse (traverse f) t <*> pure p <*> pure d <*> pure n

-- | Generates an empty 'KdMap' with a user-specified distance function.
emptyKdMapWithDistFn :: PointAsListFn a p -> SquaredDistanceFn a p -> KdMap a p v
emptyKdMapWithDistFn p2l d2 = KdMap [] p2l d2 0

-- | Returns whether the 'KdMap' is empty.
null :: KdMap a p v -> Bool
null (KdMap [] _ _ _) = True
null _ = False

-- | Generates a 'KdMap' with a single point-value pair using a
-- user-specified distance function.
singletonWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> (p, v) -> KdMap a p v
singletonWithDistFn p2l d2 (k, v) =
  KdMap [KDM.buildKdMapWithDistFn p2l d2 [(k, v)]] p2l d2 1

-- | Generates an empty 'KdMap' with the default distance function.
emptyKdMap :: Real a => PointAsListFn a p -> KdMap a p v
emptyKdMap p2l = emptyKdMapWithDistFn p2l $ defaultDistSqrFn p2l

-- | Generates a 'KdMap' with a single point-value pair using the
-- default distance function.
singleton :: Real a => PointAsListFn a p -> (p, v) -> KdMap a p v
singleton p2l = singletonWithDistFn p2l $ defaultDistSqrFn p2l

-- | Adds a given point-value pair to a 'KdMap'.
--
-- Average time complexity per insert for /n/ inserts: /O(log^2(n))/.
insert :: Real a => KdMap a p v -> p -> v -> KdMap a p v
insert (KdMap trees p2l d2 n) k v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = KDM.buildKdMapWithDistFn p2l d2  $ (k, v) : L.concatMap KDM.assocs ones
  in  KdMap (newTree : theRest) p2l d2 $ n + 1

-- | Given a 'KdMap' and a query point, returns the point-value pair in
-- the 'KdMap' with the point nearest to the query.
--
-- Average time complexity: /O(log^2(n))/.
nearestNeighbor :: Real a => KdMap a p v -> p -> (p, v)
nearestNeighbor (KdMap ts _ d2 _) query =
  let nearests = map (`KDM.nearestNeighbor` query) ts
  in  if   Data.List.null nearests
      then error "Called nearestNeighbor on empty KdMap."
      else L.minimumBy (compare `on` (d2 query . fst)) nearests

insertPair :: Real a => KdMap a p v -> (p, v) -> KdMap a p v
insertPair t = uncurry (insert t)

-- | Given a 'KdMap', a query point, and a number @k@, returns the
-- @k@ point-value pairs with the nearest points to the query.
--
-- Neighbors are returned in order of increasing distance from query
-- point.
--
-- Average time complexity: /log(k) * log^2(n)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
--
-- Worst case time complexity: /n * log(k)/ for /k/ nearest neighbors
-- on a structure with /n/ data points.
kNearestNeighbors :: Real a => KdMap a p v -> Int -> p -> [(p, v)]
kNearestNeighbors (KdMap trees _ d2 _) k query =
  let neighborSets = map (\t -> KDM.kNearestNeighbors t k query) trees
  in  take k $ L.foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = d2 query $ fst x
              distY = d2 query $ fst y

-- | Given a 'KdMap', a query point, and a radius, returns all
-- point-value pairs in the 'KdTree' with points within the given
-- radius of the query point.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
pointsInRadius :: Real a => KdMap a p v -> a -> p -> [(p, v)]
pointsInRadius (KdMap trees _ _ _) radius query =
  L.concatMap (\t -> KDM.pointsInRadius t radius query) trees

-- | Finds all point-value pairs in a 'KdMap' with points within a
-- given range, where the range is specified as a set of lower and
-- upper bounds.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for n data points and a range
-- that spans all the points.
pointsInRange :: Real a => KdMap a p v
                           -> p -- ^ lower bounds of range
                           -> p -- ^ upper bounds of range
                           -> [(p, v)] -- ^ point-value pairs within
                                       -- given range
pointsInRange (KdMap trees _ _ _) lowers uppers =
  L.concatMap (\t -> KDM.pointsInRange t lowers uppers) trees

-- | Returns the number of elements in the 'KdMap'.
--
-- Time complexity: /O(1)/
size :: KdMap a p v -> Int
size (KdMap _ _ _ n) = n

-- | Returns a list of all the point-value pairs in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
assocs :: KdMap a p v -> [(p, v)]
assocs (KdMap trees _ _ _) = L.concatMap KDM.assocs trees

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

-- | Inserts a list of point-value pairs into the 'KdMap'.
batchInsert :: Real a => KdMap a p v -> [(p, v)] -> KdMap a p v
-- TODO: This can be made far more efficient by batch-creating the
-- individual KdMaps before placing them into the KdMap
batchInsert =  L.foldl' insertPair

-- | Returns size of each internal /k/-d tree that makes up the
-- dynamic structure. For internal testing use.
subtreeSizes :: KdMap a p v -> [Int]
subtreeSizes (KdMap trees _ _ _) = map KDM.size trees
