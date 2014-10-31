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

newtype DkdTree p = DkdTree (DKDM.DkdMap p ())

instance Foldable DkdTree where
  foldr f z (DkdTree dkdMap) = DKDM.foldrDkdMap (f . fst) z dkdMap

emptyDkdTree :: PointAsListFn p -> SquaredDistanceFn p -> DkdTree p
emptyDkdTree p2l d2 = DkdTree $ DKDM.emptyDkdMap p2l d2

null :: DkdTree p -> Bool
null (DkdTree dkdMap) = DKDM.null dkdMap

singleton :: PointAsListFn p -> SquaredDistanceFn p -> p -> DkdTree p
singleton p2l d2 p = DkdTree $ DKDM.singleton p2l d2 (p, ())

insert :: DkdTree p -> p -> DkdTree p
insert (DkdTree dkdMap) p = DkdTree $ DKDM.insert dkdMap p ()

nearestNeighbor :: DkdTree p -> p -> p
nearestNeighbor (DkdTree dkdMap) = fst . DKDM.nearestNeighbor dkdMap

kNearestNeighbors :: DkdTree p -> Int -> p -> [p]
kNearestNeighbors (DkdTree dkdMap) k query =
  map fst $ DKDM.kNearestNeighbors dkdMap k query

nearNeighbors :: DkdTree p -> Double -> p -> [p]
nearNeighbors (DkdTree dkdMap) radius query =
  map fst $ DKDM.nearNeighbors dkdMap radius query

size :: DkdTree p -> Int
size (DkdTree dkdMap) = DKDM.size dkdMap

points :: DkdTree p -> [p]
points (DkdTree dkdMap) = DKDM.keys dkdMap
