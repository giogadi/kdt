module Data.Trees.KdTree
       ( KdSpace (..)
       , KdTree
       , buildKdTree
       , nearestNeighbor
       , nearNeighbors
       , kNearestNeighbors
       , toList
       , size
       ) where

import qualified Data.Trees.KdMap as KDM
import Data.Trees.KdMap (KdSpace (..))

type KdTree p = KDM.KdMap p ()

buildKdTree :: KdSpace p -> [p] -> KdTree p
buildKdTree _ [] = error "KdTree must be build with a non-empty list."
buildKdTree s ps = KDM.buildKdMap s $ zip ps $ repeat ()

nearestNeighbor :: KdTree p -> p -> p
nearestNeighbor t query = fst $ KDM.nearestNeighbor t query

nearNeighbors :: KdTree p -> Double -> p -> [p]
nearNeighbors t radius query = map fst $ KDM.nearNeighbors t radius query

kNearestNeighbors :: KdTree p -> Int -> p -> [p]
kNearestNeighbors t k query = map fst $ KDM.kNearestNeighbors t k query

toList :: KdTree p -> [p]
toList = map fst . KDM.toList

size :: KdTree p -> Int
size = KDM.size
