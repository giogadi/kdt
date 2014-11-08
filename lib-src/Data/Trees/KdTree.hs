{-# LANGUAGE DeriveGeneric #-}

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
       , Point2d (..)
       , pointAsList2d
       , distSqr2d
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import Data.Foldable

import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (PointAsListFn,
                         SquaredDistanceFn,
                         Point2d (..),
                         pointAsList2d,
                         distSqr2d)

newtype KdTree a p = KdTree (KDM.KdMap a p ()) deriving Generic
instance (NFData a, NFData p) => NFData (KdTree a p) where rnf = genericRnf

instance Foldable (KdTree a) where
  foldr f z (KdTree kdMap) = KDM.foldrKdMap (f . fst) z kdMap

buildKdTree :: Real a => PointAsListFn a p -> [p] -> KdTree a p
buildKdTree _ [] = error "KdTree must be built with a non-empty list."
buildKdTree pointAsList ps =
  KdTree $ KDM.buildKdMap pointAsList $ zip ps $ repeat ()

buildKdTreeWithDistFn :: Real a =>
                         PointAsListFn a p ->
                         SquaredDistanceFn a p ->
                         [p] ->
                         KdTree a p
buildKdTreeWithDistFn _ _ [] = error "KdTree must be built with a non-empty list."
buildKdTreeWithDistFn pointAsList distSqr ps =
  KdTree $ KDM.buildKdMapWithDistFn pointAsList distSqr $ zip ps $ repeat ()

nearestNeighbor :: Real a => KdTree a p -> p -> p
nearestNeighbor (KdTree t) query = fst $ KDM.nearestNeighbor t query

nearNeighbors :: Real a => KdTree a p -> a -> p -> [p]
nearNeighbors (KdTree t) radius query = map fst $ KDM.nearNeighbors t radius query

kNearestNeighbors :: Real a => KdTree a p -> Int -> p -> [p]
kNearestNeighbors (KdTree t) k query = map fst $ KDM.kNearestNeighbors t k query

points :: KdTree a p -> [p]
points (KdTree t) = KDM.keys t

size :: KdTree a p -> Int
size (KdTree t) = KDM.size t
