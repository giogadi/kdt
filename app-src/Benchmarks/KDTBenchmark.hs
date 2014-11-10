import Data.Point2d
import Data.KdTree.Static as KDT
import Data.KdTree.Dynamic as DKDT

import Control.Monad
import qualified Control.Monad.Random as CMR
import Criterion.Main
import Data.List
import qualified Data.PQueue.Prio.Max as Q
import System.Random.Mersenne.Pure64

zeroOnePointSampler :: CMR.Rand PureMT Point2d
zeroOnePointSampler =
  liftM2 Point2d
    (CMR.getRandomR (0.0, 1.0))
    (CMR.getRandomR (0.0, 1.0))

-- Input: List of pairs of points, where first of each pair is the
-- point to add to the dynamic KdTree, and the second is the point to
-- query for nearest neighbor
interleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
interleaveBuildQuery =
  let f :: (DKDT.KdTree Double Point2d, [Point2d]) ->
           (Point2d, Point2d) ->
           (DKDT.KdTree Double Point2d, [Point2d])
      f (kdt, accList) (treePt, queryPt) =
        let newKdt = DKDT.insert kdt treePt
            nearest = DKDT.nearestNeighbor newKdt queryPt
        in  (newKdt, nearest : accList)
      start = (DKDT.emptyKdTreeWithDistFn pointAsList2d distSqr2d, [])
  in  snd . foldl' f start

-- nn implemented with optimized linear scan
nearestLinear :: [Point2d] -> Point2d -> Point2d
nearestLinear [] _ = error "nearestLinear called on an empty list!"
nearestLinear (ph : pt) query = fst $ foldl' f (ph, distSqr2d query ph) pt
  where {-# INLINE f #-}
        f b@(_, dBest) x
          | d < dBest = (x, d)
          | otherwise = b
          where d = distSqr2d query x

nearNeighborsLinear :: [Point2d] -> Double -> Point2d -> [Point2d]
nearNeighborsLinear ps radius query =
  filter ((<= radius * radius) . distSqr2d query) ps

-- knn implemented with priority queue
kNearestNeighborsLinear :: [Point2d] -> Int -> Point2d -> [Point2d]
kNearestNeighborsLinear ps k query = reverse $ map snd $ Q.toList $ foldl' f Q.empty ps
  where f q p = let insertBounded queue dist x
                      | Q.size queue < k = Q.insert dist x queue
                      | otherwise = if dist < fst (Q.findMax queue)
                                    then Q.insert dist x $ Q.deleteMax queue
                                    else queue
                in  insertBounded q (distSqr2d query p) p

linearInterleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
linearInterleaveBuildQuery =
  let f :: ([Point2d], [Point2d]) -> (Point2d, Point2d) -> ([Point2d], [Point2d])
      f (ps, accList) (structPt, queryPt) =
        let ps' = structPt : ps
            nearest = nearestLinear ps' queryPt
        in  (ps', nearest : accList)
  in  snd . foldl' f ([], [])

main :: IO ()
main =
  let seed = 1
      treePoints = CMR.evalRand (sequence $ repeat zeroOnePointSampler) $ pureMT seed
      kdtN n = KDT.buildKdTreeWithDistFn pointAsList2d distSqr2d $ take n treePoints
      queryPoints = CMR.evalRand (sequence $ repeat zeroOnePointSampler) $ pureMT (seed + 1)
      buildKdtBench n = bench (show n) $ nf kdtN n
      nnKdtBench nq np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq) $
          nf (map (KDT.nearestNeighbor (kdtN np))) (take nq queryPoints)
      nearKdtBench nq r np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-r-" ++ show r) $
          nf (concatMap (KDT.nearNeighbors (kdtN np) r)) (take nq queryPoints)
      knnKdtBench nq k np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-k-" ++ show k) $
          nf (concatMap (KDT.kNearestNeighbors (kdtN np) k)) (take nq queryPoints)
      nnLinearBench nq np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq) $
          nf (map (nearestLinear (take np treePoints))) (take nq queryPoints)
      nearLinearBench nq r np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-r-" ++ show r) $
          nf (map $ nearNeighborsLinear (take np treePoints) r) (take nq queryPoints)
      knnLinearBench nq k np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-k-" ++ show k) $
          nf (map $ kNearestNeighborsLinear (take np treePoints) k) (take nq queryPoints)
      nniDkdtBench n =
        bench ("n-" ++ show n) $
          nf interleaveBuildQuery (zip (take n treePoints) (take n queryPoints))
      numQueries = 100
      pointSetSizes = [100, 1000, 10000, 100000]
      radius = 0.05
      numNeighbors = 10
  in  defaultMain [
      bgroup "linear-nn" $ map (nnLinearBench numQueries) pointSetSizes,
      bgroup "linear-near" $ map (nearLinearBench numQueries radius) pointSetSizes,
      bgroup "linear-knn" $ map (knnLinearBench numQueries numNeighbors) pointSetSizes,
      bgroup "kdt-build" $ map buildKdtBench pointSetSizes,
      bgroup "kdt-nn" $ map (nnKdtBench numQueries) pointSetSizes,
      bgroup "kdt-near" $ map (nearKdtBench numQueries radius) pointSetSizes,
      bgroup "kdt-knn" $ map (knnKdtBench numQueries numNeighbors) pointSetSizes,
      bgroup "dkdt-nn" $ map nniDkdtBench pointSetSizes
      ]
