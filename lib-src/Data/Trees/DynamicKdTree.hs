module Data.Trees.DynamicKdTree
       ( PointAsListFn
       , SquaredDistanceFn
       , DkdTree
       , emptyDkdTree
       , null
       , singleton
       , insert
       , nearestNeighbor
       , kNearestNeighbors
       , nearNeighbors
       , size
       , points
       ) where

import Prelude hiding (null)

import Data.Foldable

import qualified Data.Trees.DynamicKdMap as DKDM
import Data.Trees.DynamicKdMap (PointAsListFn, SquaredDistanceFn)

newtype DkdTree a p = DkdTree (DKDM.DkdMap a p ())

instance Foldable (DkdTree a) where
  foldr f z (DkdTree dkdMap) = DKDM.foldrDkdMap (f . fst) z dkdMap

emptyDkdTree :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> DkdTree a p
emptyDkdTree p2l d2 = DkdTree $ DKDM.emptyDkdMap p2l d2

null :: DkdTree a p -> Bool
null (DkdTree dkdMap) = DKDM.null dkdMap

singleton :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> p -> DkdTree a p
singleton p2l d2 p = DkdTree $ DKDM.singleton p2l d2 (p, ())

insert :: Real a => DkdTree a p -> p -> DkdTree a p
insert (DkdTree dkdMap) p = DkdTree $ DKDM.insert dkdMap p ()

nearestNeighbor :: Real a => DkdTree a p -> p -> p
nearestNeighbor (DkdTree dkdMap) = fst . DKDM.nearestNeighbor dkdMap

kNearestNeighbors :: Real a => DkdTree a p -> Int -> p -> [p]
kNearestNeighbors (DkdTree dkdMap) k query =
  map fst $ DKDM.kNearestNeighbors dkdMap k query

nearNeighbors :: Real a => DkdTree a p -> a -> p -> [p]
nearNeighbors (DkdTree dkdMap) radius query =
  map fst $ DKDM.nearNeighbors dkdMap radius query

size :: DkdTree a p -> Int
size (DkdTree dkdMap) = DKDM.size dkdMap

points :: DkdTree a p -> [p]
points (DkdTree dkdMap) = DKDM.keys dkdMap
