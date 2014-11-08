module Data.Trees.DynKdTree
       ( PointAsListFn
       , DynKdTree
       , emptyDynKdTree
       , null
       , singleton
       , SquaredDistanceFn
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

newtype DynKdTree a p = DynKdTree (DKDM.DynKdMap a p ())

instance Foldable (DynKdTree a) where
  foldr f z (DynKdTree dkdMap) = DKDM.foldrDynKdMap (f . fst) z dkdMap

emptyDynKdTreeWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> DynKdTree a p
emptyDynKdTreeWithDistFn p2l d2 = DynKdTree $ DKDM.emptyDynKdMapWithDistFn p2l d2

emptyDynKdTree :: Real a => PointAsListFn a p -> DynKdTree a p
emptyDynKdTree p2l = emptyDynKdTreeWithDistFn p2l $ defaultDistSqrFn p2l

null :: DynKdTree a p -> Bool
null (DynKdTree dkdMap) = DKDM.null dkdMap

singletonWithDistFn :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> p -> DynKdTree a p
singletonWithDistFn p2l d2 p = DynKdTree $ DKDM.singletonWithDistFn p2l d2 (p, ())

singleton :: Real a => PointAsListFn a p -> p -> DynKdTree a p
singleton p2l = singletonWithDistFn p2l $ defaultDistSqrFn p2l

insert :: Real a => DynKdTree a p -> p -> DynKdTree a p
insert (DynKdTree dkdMap) p = DynKdTree $ DKDM.insert dkdMap p ()

nearestNeighbor :: Real a => DynKdTree a p -> p -> p
nearestNeighbor (DynKdTree dkdMap) = fst . DKDM.nearestNeighbor dkdMap

kNearestNeighbors :: Real a => DynKdTree a p -> Int -> p -> [p]
kNearestNeighbors (DynKdTree dkdMap) k query =
  map fst $ DKDM.kNearestNeighbors dkdMap k query

nearNeighbors :: Real a => DynKdTree a p -> a -> p -> [p]
nearNeighbors (DynKdTree dkdMap) radius query =
  map fst $ DKDM.nearNeighbors dkdMap radius query

size :: DynKdTree a p -> Int
size (DynKdTree dkdMap) = DKDM.size dkdMap

points :: DynKdTree a p -> [p]
points (DynKdTree dkdMap) = DKDM.keys dkdMap
