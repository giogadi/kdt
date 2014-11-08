module Data.KdTree.Dynamic
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdTree
         -- ** Dynamic /k/-d tree construction
       , emptyKdTree
       , singleton
       , emptyKdTreeWithDistFn
       , singletonWithDistFn
         -- ** Insertion
       , insert
         -- ** Query
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , points
       , null
       , size
         -- ** Utilities
       , defaultDistSqrFn
       ) where

import Prelude hiding (null)

import Data.Foldable

import qualified Data.KdMap.Dynamic as DKDM
import Data.KdMap.Dynamic (PointAsListFn, SquaredDistanceFn, defaultDistSqrFn)

-- $usage
--
-- The 'KdTree' is a dynamic variant of
-- @Data.KdTree.Static.@'Data.KdTree.Static.KdTree' that allows for
-- insertion of new points into an existing 'KdTree'. This algorithm
-- was implemented using a
-- <http://repository.cmu.edu/cgi/viewcontent.cgi?article=3453&context=compsci static-to-dynamic transformation>.
--
-- Here's an example of interleaving 3D point insertions and point
-- queries using 'KdTree':
--
-- @
-- >>> let dkdt = singleton point3dAsList (Point3D 0.0 0.0 0.0)
--
-- >>> let dkdt' = insert dkdt (Point3D 1.0 1.0 1.0)
--
-- >>> nearestNeighbor dkdt' (Point3D 0.4 0.4 0.4)
-- Point3D {x = 0.0, y = 0.0, z = 0.0}
--
-- >>> let dkdt'' = insert dkdt' (Point3D 0.5 0.5 0.5)
--
-- >>> nearestNeighbor dkdt'' (Point3D 0.4 0.4 0.4)
-- Point3D {x = 0.5, y = 0.5, z = 0.5}
-- @
--
-- Check out @Data.KdMap.Dynamic.@'Data.KdMap.Dynamic.KdMap' if you want to associate a value
-- with each point in your tree structure.

-- | A dynamic /k/-d tree structure that stores points of type @p@
-- with axis values of type @a@.
newtype KdTree a p = KdTree (DKDM.KdMap a p ())

instance Foldable (KdTree a) where
  foldr f z (KdTree dkdMap) = DKDM.foldrKdMap (f . fst) z dkdMap

-- | Generates an empty 'KdTree' with a user-specified distance function.
emptyKdTreeWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> KdTree a p
emptyKdTreeWithDistFn p2l d2 = KdTree $ DKDM.emptyKdMapWithDistFn p2l d2

-- | Generates an empty 'KdTree' with the default distance function.
emptyKdTree :: Real a => PointAsListFn a p -> KdTree a p
emptyKdTree p2l = emptyKdTreeWithDistFn p2l $ defaultDistSqrFn p2l

-- | Returns whether the 'KdTree' is empty.
null :: KdTree a p -> Bool
null (KdTree dkdMap) = DKDM.null dkdMap

-- | Generates a 'KdTree' with a single point using a
-- user-specified distance function.
singletonWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> p -> KdTree a p
singletonWithDistFn p2l d2 p = KdTree $ DKDM.singletonWithDistFn p2l d2 (p, ())

-- | Generates a 'KdTree' with a single point using the default
-- distance function.
singleton :: Real a => PointAsListFn a p -> p -> KdTree a p
singleton p2l = singletonWithDistFn p2l $ defaultDistSqrFn p2l

-- | Adds a given point to a 'KdTree'.
--
-- Average time complexity per insert for /n/ inserts: /O(log^2(n))/.
insert :: Real a => KdTree a p -> p -> KdTree a p
insert (KdTree dkdMap) p = KdTree $ DKDM.insert dkdMap p ()

-- | Given a 'KdTree' and a query point, returns the nearest point
-- in the 'KdTree' to the query point.
--
-- Average time complexity: /O(log^2(n))/.
nearestNeighbor :: Real a => KdTree a p -> p -> p
nearestNeighbor (KdTree dkdMap) = fst . DKDM.nearestNeighbor dkdMap

-- | Given a 'KdTree', a query point, and a number @k@, returns the
-- @k@ nearest points in the 'KdTree' to the query point.
--
-- TODO: time complexity.
kNearestNeighbors :: Real a => KdTree a p -> Int -> p -> [p]
kNearestNeighbors (KdTree dkdMap) k query =
  map fst $ DKDM.kNearestNeighbors dkdMap k query

-- | Given a 'KdTree', a query point, and a radius, returns all
-- points in the 'KdTree' that are within the given radius of the
-- query points.
--
-- TODO: time complexity.
nearNeighbors :: Real a => KdTree a p -> a -> p -> [p]
nearNeighbors (KdTree dkdMap) radius query =
  map fst $ DKDM.nearNeighbors dkdMap radius query

-- | Returns the number of elements in the 'KdTree'.
--
-- Time complexity: /O(1)/
size :: KdTree a p -> Int
size (KdTree dkdMap) = DKDM.size dkdMap

-- | Returns a list of all the points in the 'KdTree'.
--
-- Time complexity: /O(n)/
points :: KdTree a p -> [p]
points (KdTree dkdMap) = DKDM.points dkdMap
