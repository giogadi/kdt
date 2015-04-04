module Data.VPMap.Dynamic
       ( DistanceFn
       , VPMap
       , empty
       , singleton
       , insert
       , insertPair
       , batchInsert
       , nearest
       , inRadius
       , kNearest
       , assocs
       , keys
       , elems
       , null
       , size
       , subtreeSizes
       ) where

import Data.Bits
import Data.Function
import qualified Data.List as L hiding (insert)
import Prelude hiding (null)

import qualified Data.VPMap.Static as VPM
import Data.VPMap.Static (DistanceFn)

data VPMap a p v = VPMap
                   { _trees :: [VPM.VPMap a p v]
                   , _dist  :: DistanceFn a p
                   , _size  :: Int
                   }

empty :: Real a => DistanceFn a p -> VPMap a p v
empty dist = VPMap [] dist 0

null :: VPMap a p v -> Bool
null (VPMap [] _ _) = True
null _ = False

singleton :: Real a => DistanceFn a p -> (p, v) -> VPMap a p v
singleton dist point = VPMap [VPM.build dist [point]] dist 1

insert :: Real a => VPMap a p v -> p -> v -> VPMap a p v
insert (VPMap trees dist n) p v =
  let bitList = map ((1 .&.) . (n `shiftR`)) [0..]
      (onesPairs, theRestPairs) = span ((== 1) . fst) $ zip bitList trees
      ((_, ones), (_, theRest)) = (unzip onesPairs, unzip theRestPairs)
      newTree = VPM.build dist  $ (p, v) : L.concatMap VPM.assocs ones
  in  VPMap (newTree : theRest) dist $ n + 1

insertPair :: Real a => VPMap a p v -> (p, v) -> VPMap a p v
insertPair t = uncurry (insert t)

nearest :: Real a => VPMap a p v -> p -> (p, v)
nearest (VPMap ts dist _) query =
  let nearests = map (`VPM.nearest` query) ts
  in  if   L.null nearests
      then error "Called nearest on empty VPMap."
      -- TODO do this the more efficient way
      else L.minimumBy (compare `on` (dist query . fst)) nearests

kNearest :: Real a => VPMap a p v -> Int -> p -> [(p, v)]
kNearest (VPMap trees dist _) k query =
  let neighborSets = map (\t -> VPM.kNearest t k query) trees
  in  take k $ L.foldr merge [] neighborSets
 where merge [] ys = ys
       merge xs [] = xs
       merge xs@(x:xt) ys@(y:yt)
         | distX <= distY = x : merge xt ys
         | otherwise      = y : merge xs yt
        where distX = dist query $ fst x
              distY = dist query $ fst y

inRadius :: Real a => VPMap a p v -> a -> p -> [(p, v)]
inRadius (VPMap trees _ _) radius query =
  L.concatMap (\t -> VPM.inRadius t radius query) trees

size :: VPMap a p v -> Int
size (VPMap _ _ n) = n

assocs :: VPMap a p v -> [(p, v)]
assocs (VPMap trees _ _) = L.concatMap VPM.assocs trees

keys :: VPMap a p v -> [p]
keys = map fst . assocs

elems :: VPMap a p v -> [v]
elems = map snd . assocs

batchInsert :: Real a => VPMap a p v -> [(p, v)] -> VPMap a p v
batchInsert = L.foldl' insertPair

subtreeSizes :: VPMap a p v -> [Int]
subtreeSizes (VPMap trees _ _) = map VPM.size trees
