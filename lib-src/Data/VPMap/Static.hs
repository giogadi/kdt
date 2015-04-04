{-# LANGUAGE FlexibleContexts #-}

module Data.VPMap.Static
       ( DistanceFn
       , build
       , VPMap
       , assocs
       , size
       , nearest
       , inRadius
       , kNearest
       , isValid
       ) where

import qualified Data.List as L

import qualified Data.Heap as Q
import Data.Internal (quickselect)
import Data.Maybe

data TreeNode a p v = TreeNode { _treeNear :: TreeNode a p v
                               , _treePoint :: (p, v)
                               , _nearDistance :: a
                               , _treeFar :: TreeNode a p v
                               }
                    | Empty

type DistanceFn a p = p -> p -> a

buildTreeNode :: Real a => DistanceFn a p -> [(p, v)] -> TreeNode a p v
buildTreeNode _ [] = Empty
-- Is 0.0 a safe distance here? Want to ensure that queries *always*
-- check this node if they get here
--
-- Also is there a better way to get zero dist?
buildTreeNode dist [pivot] =
  TreeNode Empty pivot (dist (fst pivot) (fst pivot)) Empty
buildTreeNode dist (pivot@(p, _) : dataPoints) =
  let n = length dataPoints
      -- Use first element as pivot because why the hell not
      distsFromPivot = map (dist p . fst) dataPoints
      medianDist = quickselect compare (n `div` 2) distsFromPivot
      distPointPairs = zip distsFromPivot dataPoints
      (closerThanMedian, fartherThanMedian) =
        L.partition ((< medianDist) . fst) distPointPairs
  in  TreeNode
      { _treeNear = buildTreeNode dist $ snd $ unzip closerThanMedian
      , _treePoint = pivot
      , _nearDistance = medianDist
      , _treeFar = buildTreeNode dist $ snd $ unzip fartherThanMedian
      }

assocsInternal :: TreeNode a p v -> [(p, v)]
assocsInternal t = go t []
  where go Empty = id
        go (TreeNode n p _ f) = go n . (p :) . go f

isTreeNodeValid :: Real a => DistanceFn a p -> TreeNode a p v -> Bool
isTreeNodeValid _ Empty = True
isTreeNodeValid dist (TreeNode near (p, _) nearDist far) =
  let treeDists = map (dist p . fst) . assocsInternal
  in  all (< nearDist) (treeDists near) &&
      all (>= nearDist) (treeDists far) &&
      isTreeNodeValid dist near && isTreeNodeValid dist far

data VPMap a p v = VPMap { _dist     :: DistanceFn a p
                         , _rootNode :: TreeNode a p v
                         , _size     :: Int
                         }

build :: Real a => DistanceFn a p -> [(p, v)] -> VPMap a p v
build dist dataPoints = VPMap { _dist = dist
                              , _rootNode = buildTreeNode dist dataPoints
                              , _size = length dataPoints
                              }

assocs :: VPMap a p v -> [(p, v)]
assocs vpm = assocsInternal $ _rootNode vpm

isValid :: Real a => VPMap a p v -> Bool
isValid vpm = isTreeNodeValid (_dist vpm) $ _rootNode vpm

size :: VPMap a p v -> Int
size = _size

nearest :: Real a => VPMap a p v -> p -> (p, v)
nearest (VPMap _ Empty _) _ =
  error "Attempted to call nearest on an empty VPMap."
nearest (VPMap dist root _) query =
  let go bestSoFar Empty = bestSoFar
      go (bestDistSoFar, bestSoFar) (TreeNode near (p, v) nearDist far) =
        let pivotDist = dist p query
            checkNear = pivotDist < nearDist + bestDistSoFar
            better x1@(dist1, _) x2@(dist2, _) = if dist1 < dist2
                                                 then x1
                                                 else x2
            bestAfterPivot =
              better (pivotDist, (p, v)) (bestDistSoFar, bestSoFar)
            bestAfterNear = if checkNear
                            then go bestAfterPivot near
                            else bestAfterPivot
            checkFar = pivotDist >= nearDist - (fst bestAfterNear)
            bestAfterFar = if checkFar
                           then go bestAfterNear far
                           else bestAfterNear
        in  bestAfterFar
  in  snd $ go (dist (fst $ _treePoint root) query, _treePoint root) root

inRadius :: Real a => VPMap a p v
                   -> a
                   -> p
                   -> [(p, v)]
inRadius (VPMap dist root _) radius query =
  let go Empty acc = acc
      go (TreeNode near (p, v) nearDist far) acc =
        let pivotDist = dist p query
            accAfterPivot = if pivotDist <= radius
                            then (p, v) : acc
                            else acc
            checkNear = pivotDist < nearDist + radius
            accAfterNear = if checkNear
                           then go near accAfterPivot
                           else accAfterPivot
            checkFar = pivotDist >= nearDist - radius
            accAfterFar = if checkFar
                          then go far accAfterNear
                          else accAfterNear
        in  accAfterFar
  in  go root []

kNearest :: Real a => VPMap a p v
                   -> Int
                   -> p
                   -> [(p, v)]
kNearest (VPMap dist root _) numNeighbors query =
  let go Empty queue = queue
      go (TreeNode near (p, v) nearDist far) queue =
        let insertBounded q d x
              | Q.size q < numNeighbors = Q.insert (d, x) q
              | otherwise = let ((farthestDist, _), rest) = fromJust $ Q.view q
                            in  if d < farthestDist
                                then Q.insert (d, x) rest
                                else q
            pivotDist = dist p query
            queueAfterPivot = insertBounded queue pivotDist (p, v)
            maxDistAfterPivot = (fst . fst) $ fromJust $ Q.view queueAfterPivot
            checkNear = Q.size queueAfterPivot < numNeighbors ||
                        pivotDist < nearDist + maxDistAfterPivot
            queueAfterNear = if checkNear
                             then go near queueAfterPivot
                             else queueAfterPivot
            maxDistAfterNear = (fst . fst) $ fromJust $ Q.view queueAfterNear
            checkFar = Q.size queueAfterNear < numNeighbors ||
                       pivotDist >= nearDist - maxDistAfterNear
            queueAfterFar = if checkFar
                            then go far queueAfterNear
                            else queueAfterNear
        in  queueAfterFar
  in  reverse $ map snd $ Q.toAscList $
        go root (Q.empty :: Q.MaxPrioHeap a (p, v))
