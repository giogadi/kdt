module Data.VPMap.Static
       ( DistanceFn
       , build
       , VPMap
       , assocs
       , size
       , isValid
       ) where

import qualified Data.List as L

import Data.Internal (quickselect)

data TreeNode a p v = TreeNode { _treeNear :: TreeNode a p v
                               , _treePoint :: (p, v)
                               , _nearDistance :: a
                               , _treeFar :: TreeNode a p v
                               }
                    | Empty

type DistanceFn a p = p -> p -> a

buildTreeNode :: Real a => DistanceFn a p -> [(p, v)] -> TreeNode a p v
buildTreeNode _ [] = Empty
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
