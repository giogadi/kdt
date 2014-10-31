module Data.Trees.KdTree
       ( PointAsListFn
       , SquaredDistanceFn
       , KdTree
       , buildKdTree
       , buildKdTreeWithDistFn
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , points
       , size
       ) where

import Data.Foldable

import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (PointAsListFn, SquaredDistanceFn)

newtype KdTree p = KdTree (KDM.KdMap p ())

instance Foldable KdTree where
  foldr f z (KdTree kdMap) = KDM.foldrKdMap (f . fst) z kdMap

buildKdTree :: PointAsListFn p -> [p] -> KdTree p
buildKdTree _ [] = error "KdTree must be built with a non-empty list."
buildKdTree pointAsList ps =
  KdTree $ KDM.buildKdMap pointAsList $ zip ps $ repeat ()

buildKdTreeWithDistFn :: PointAsListFn p -> SquaredDistanceFn p -> [p] -> KdTree p
buildKdTreeWithDistFn _ _ [] = error "KdTree must be built with a non-empty list."
buildKdTreeWithDistFn pointAsList distSqr ps =
  KdTree $ KDM.buildKdMapWithDistFn pointAsList distSqr $ zip ps $ repeat ()

nearestNeighbor :: KdTree p -> p -> p
nearestNeighbor (KdTree t) query = fst $ KDM.nearestNeighbor t query

nearNeighbors :: KdTree p -> Double -> p -> [p]
nearNeighbors (KdTree t) radius query = map fst $ KDM.nearNeighbors t radius query

kNearestNeighbors :: KdTree p -> Int -> p -> [p]
kNearestNeighbors (KdTree t) k query = map fst $ KDM.kNearestNeighbors t k query

points :: KdTree p -> [p]
points (KdTree t) = KDM.keys t

size :: KdTree p -> Int
size (KdTree t) = KDM.size t
