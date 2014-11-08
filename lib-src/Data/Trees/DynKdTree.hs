module Data.Trees.DynKdTree
       ( -- * Usage

         -- $usage

         -- * Reference

         PointAsListFn
       , DynKdTree
       , emptyDynKdTree
       , null
       , singleton
       , SquaredDistanceFn
       , defaultDistSqrFn
       , emptyDynKdTreeWithDistFn
       , singletonWithDistFn
       , insert
       , nearestNeighbor
       , kNearestNeighbors
       , nearNeighbors
       , size
       , points
       ) where

import Prelude hiding (null)

import Data.Foldable

import qualified Data.Trees.DynKdMap as DKDM
import Data.Trees.DynKdMap (PointAsListFn, SquaredDistanceFn, defaultDistSqrFn)

-- $usage
--
-- The 'DynKdTree' is a dynamic variant of 'KdTree' that allows for
-- insertion of new points into an existing 'KdTree'. This algorithm
-- was implemented using a
-- <http://repository.cmu.edu/cgi/viewcontent.cgi?article=3453&context=compsci static-to-dynamic transformation>.
--
-- Here's an example of interleaving 3D point insertions and point
-- queries using 'DynKdTree':
--
-- @
-- >>> let dkdt = singleton point3dAsList (Point3D 0.0 0.0 0.0)
--
-- >>> let dkdt' = insert dkdt (Point3D 1.0 1.0 1.0)
--
-- >>> nearestNeighbor dkdt' (Point3D 0.4 0.4 0.4)
-- Point3D 0.0 0.0 0.0
--
-- >>> let dkdt'' = insert dkdt' (Point3D 0.5 0.5 0.5)
--
-- >>> nearestNeighbor dkdt'' (Point3D 0.4 0.4 0.4)
-- Point3D 0.5 0.5 0.5
-- @
--
-- Check out the 'Data.Trees.DynKdMap' module if you want to associate a value
-- with each point in your tree structure.

-- | A dynamic /k/-d tree structure that stores points of type @p@
-- with axis values of type @a@.
newtype DynKdTree a p = DynKdTree (DKDM.DynKdMap a p ())

instance Foldable (DynKdTree a) where
  foldr f z (DynKdTree dkdMap) = DKDM.foldrDynKdMap (f . fst) z dkdMap

-- | Generates an empty 'DynKdTree' with a user-specified distance function.
emptyDynKdTreeWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> DynKdTree a p
emptyDynKdTreeWithDistFn p2l d2 = DynKdTree $ DKDM.emptyDynKdMapWithDistFn p2l d2

-- | Generates an empty 'DynKdTree' with the default distance function.
emptyDynKdTree :: Real a => PointAsListFn a p -> DynKdTree a p
emptyDynKdTree p2l = emptyDynKdTreeWithDistFn p2l $ defaultDistSqrFn p2l

-- | Returns whether the 'DynKdTree' is empty.
null :: DynKdTree a p -> Bool
null (DynKdTree dkdMap) = DKDM.null dkdMap

-- | Generates a 'DynKdTree' with a single point using a
-- user-specified distance function.
singletonWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> p -> DynKdTree a p
singletonWithDistFn p2l d2 p = DynKdTree $ DKDM.singletonWithDistFn p2l d2 (p, ())

-- | Generates a 'DynKdTree' with a single point using the default
-- distance function.
singleton :: Real a => PointAsListFn a p -> p -> DynKdTree a p
singleton p2l = singletonWithDistFn p2l $ defaultDistSqrFn p2l

-- | Adds a given point to a 'DynKdTree'.
--
-- Average time complexity per insert for /n/ inserts: /O(log^2(n))/.
insert :: Real a => DynKdTree a p -> p -> DynKdTree a p
insert (DynKdTree dkdMap) p = DynKdTree $ DKDM.insert dkdMap p ()

-- | Given a 'DynKdTree' and a query point, returns the nearest point
-- in the 'DynKdTree' to the query point.
--
-- Average time complexity: /O(log^2(n))/.
nearestNeighbor :: Real a => DynKdTree a p -> p -> p
nearestNeighbor (DynKdTree dkdMap) = fst . DKDM.nearestNeighbor dkdMap

-- | Given a 'DynKdTree', a query point, and a number @k@, returns the
-- @k@ nearest points in the 'DynKdTree' to the query point.
--
-- TODO: time complexity.
kNearestNeighbors :: Real a => DynKdTree a p -> Int -> p -> [p]
kNearestNeighbors (DynKdTree dkdMap) k query =
  map fst $ DKDM.kNearestNeighbors dkdMap k query

-- | Given a 'DynKdTree', a query point, and a radius, returns all
-- points in the 'DynKdTree' that are within the given radius of the
-- query points.
--
-- TODO: time complexity.
nearNeighbors :: Real a => DynKdTree a p -> a -> p -> [p]
nearNeighbors (DynKdTree dkdMap) radius query =
  map fst $ DKDM.nearNeighbors dkdMap radius query

-- | Returns the number of elements in the 'DynKdTree'.
--
-- Time complexity: /O(1)/
size :: DynKdTree a p -> Int
size (DynKdTree dkdMap) = DKDM.size dkdMap

-- | Returns a list of all the points in the 'DynKdTree'.
--
-- Time complexity: /O(n)/
points :: DynKdTree a p -> [p]
points (DynKdTree dkdMap) = DKDM.keys dkdMap
