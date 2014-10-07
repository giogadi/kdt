import Data.Trees.KdMap
import Data.Trees.DynamicKdTree

import Control.Monad
import qualified Control.Monad.Random as CMR
import Criterion.Main
import Data.List
import System.Random.Mersenne.Pure64

zeroOnePointSampler :: CMR.Rand PureMT Point2d
zeroOnePointSampler =
  liftM2 Point2d
    (CMR.getRandomR (0.0, 1.0))
    (CMR.getRandomR (0.0, 1.0))

-- Input: List of pairs of points, where first of each pair is the
-- point to add to the DkdTree, and the second is the point to query
-- for nearest neighbor
interleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
interleaveBuildQuery =
  let f :: (DkdTree Point2d (), [Point2d]) ->
           (Point2d, Point2d) ->
           (DkdTree Point2d (), [Point2d])
      f (kdt, accList) (treePt, queryPt) =
        let newKdt = Data.Trees.DynamicKdTree.insert kdt treePt ()
            (nearest, _) = Data.Trees.DynamicKdTree.nearestNeighbor newKdt queryPt
        in  (newKdt, nearest : accList)
      start = (emptyDkdTree mk2DEuclideanSpace, [])
  in  snd . foldl' f start

main :: IO ()
main =
  let seed = 1
      numPoints = 100000
      treePoints = CMR.evalRand (replicateM numPoints zeroOnePointSampler) $ pureMT seed
      kdt5000 = buildKdTree mk2DEuclideanSpace $ zip (take 5000 treePoints) $ repeat ()
      queryPoints = CMR.evalRand (replicateM numPoints zeroOnePointSampler) $ pureMT (seed + 1)
  in  defaultMain [
      bgroup "kdtree" [ bench "build-5000" $ nf
                          (buildKdMap mk2DEuclideanSpace) (zip (take 5000 treePoints) $ repeat ()),
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
                           (batchInsert $ emptyDkdTree mk2DEuclideanSpace)
                           (zip (take 5000 treePoints) $ repeat ()),
                         bench "interleave-5000" $ nf
                           interleaveBuildQuery
                           (zip (take 5000 treePoints) queryPoints)
                       ]
      ]
