import Data.Point2d
import Data.Trees.KdMap
import Data.Trees.DynKdMap

import Control.Monad
import qualified Control.Monad.Random as CMR
import Criterion.Main
import Data.Function
import Data.List
import System.Random.Mersenne.Pure64

zeroOnePointSampler :: CMR.Rand PureMT Point2d
zeroOnePointSampler =
  liftM2 Point2d
    (CMR.getRandomR (0.0, 1.0))
    (CMR.getRandomR (0.0, 1.0))

-- Input: List of pairs of points, where first of each pair is the
-- point to add to the DkdMap, and the second is the point to query
-- for nearest neighbor
interleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
interleaveBuildQuery =
  let f :: (DkdMap Double Point2d (), [Point2d]) ->
           (Point2d, Point2d) ->
           (DkdMap Double Point2d (), [Point2d])
      f (kdt, accList) (treePt, queryPt) =
        let newKdt = Data.Trees.DynKdMap.insert kdt treePt ()
            (nearest, _) = Data.Trees.DynKdMap.nearestNeighbor newKdt queryPt
        in  (newKdt, nearest : accList)
      start = (emptyDkdMap pointAsList2d distSqr2d, [])
  in  snd . foldl' f start

nearestLinear :: [Point2d] -> Point2d -> Point2d
nearestLinear ps query = minimumBy (compare `on` distSqr2d query) ps

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
      numPoints = 100000
      treePoints = CMR.evalRand (replicateM numPoints zeroOnePointSampler) $ pureMT seed
      kdt5000 = buildKdMapWithDistFn pointAsList2d distSqr2d $
                  zip (take 5000 treePoints) $ repeat ()
      queryPoints = CMR.evalRand (replicateM numPoints zeroOnePointSampler) $ pureMT (seed + 1)
  in  defaultMain [
      bgroup "linear" [ bench "build-5000-query-5000" $ nf
                          (map (nearestLinear (take 5000 treePoints))) (take 5000 queryPoints),
                        bench "interleave-5000" $ nf
                          linearInterleaveBuildQuery
                          (zip (take 5000 treePoints) (take 5000 queryPoints)),
                        bench "interleave-10000" $ nf
                          linearInterleaveBuildQuery
                          (zip (take 10000 treePoints) (take 10000 queryPoints))
                      ],
      bgroup "kdtree" [ bench "build-10000-only" $ nf
                          (buildKdMapWithDistFn pointAsList2d distSqr2d)
                          (zip (take 10000 treePoints) $ repeat ()),
                        bench "build-5000-query-5000" $ nf
                          (map (Data.Trees.KdMap.nearestNeighbor kdt5000))
                          (take 5000 queryPoints),
                        bench "build-5000-near-5000-r-0.1" $ nf
                          (map (Data.Trees.KdMap.nearNeighbors kdt5000 0.1))
                          (take 5000 queryPoints),
                        bench "build-5000-k-near-5000-k-10" $ nf
                          (map (Data.Trees.KdMap.kNearestNeighbors kdt5000 10))
                          (take 5000 queryPoints)
                      ],
      bgroup "dkdtree" [ bench "batch-5000" $ nf
                           (batchInsert $ emptyDkdMap pointAsList2d distSqr2d)
                           (zip (take 5000 treePoints) $ repeat ()),
                         bench "interleave-5000" $ nf
                           interleaveBuildQuery
                           (zip (take 5000 treePoints) (take 5000 queryPoints)),
                         bench "interleave-10000" $ nf
                           interleaveBuildQuery
                           (zip (take 10000 treePoints) (take 10000 queryPoints))
                       ]
      ]
