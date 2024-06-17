{-# LANGUAGE TemplateHaskell, CPP #-}

import qualified Data.KdMap.Static as KDM
import Data.KdMap.Dynamic

import Control.Monad (unless)
import Data.Bits
import Data.List
import qualified Data.Set as Set
import Data.Set (isSubsetOf)
import Data.Point2d
import System.Exit (exitFailure)
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

import Tests.TestHelpers (nearestsLinear, withinDistanceOfKthNearest)

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN kdm = length (subtreeSizes kdm) == popCount (size kdm)
  in  all lengthIsLogN $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: Real a => PointAsListFn a p ->
                                    SquaredDistanceFn a p ->
                                    [p] ->
                                    Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 = all ((== 1) . popCount) . subtreeSizes
  in  all sizesPowerOf2 $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, kdm) = size kdm == num && num == sum (subtreeSizes kdm)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestConsistentWithLinear :: (Eq p, Real a) => PointAsListFn a p ->
                                               SquaredDistanceFn a p ->
                                               ([p], p) ->
                                               Bool
checkNearestConsistentWithLinear p2l d2 (ps, query) =
  let dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = nearest dkdt query
  in  dkdtAnswer `elem` nearestsLinear p2l (testElements ps) query

prop_nearestConsistentWithLinear :: Point2d -> Property
prop_nearestConsistentWithLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestConsistentWithLinear pointAsList2d distSqr2d (xs, query)

checkKNearestConsistentWithLinear :: (Ord p, Real a) => PointAsListFn a p ->
                                                        SquaredDistanceFn a p ->
                                                        ([p], Int, p) ->
                                                        Bool
checkKNearestConsistentWithLinear p2l d2 (ps, k, query) =
  let dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = kNearest dkdt k query
      possibleNearest = withinDistanceOfKthNearest p2l (testElements ps) query k
  in  Set.fromList dkdtAnswer `isSubsetOf` Set.fromList possibleNearest

prop_kNearestConsistentWithLinear :: Point2d -> Property
prop_kNearestConsistentWithLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestConsistentWithLinear pointAsList2d distSqr2d (xs, k, query)

checkInRadiusEqualToBatch :: (Ord p, Real a) => PointAsListFn a p ->
                                            SquaredDistanceFn a p ->
                                            ([p], a, p) ->
                                            Bool
checkInRadiusEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildWithDist p2l d2 $ testElements ps
      kdtAnswer = KDM.inRadius kdt radius query
      dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = inRadius dkdt radius query
  in  sort dkdtAnswer == sort kdtAnswer

prop_checkInRadiusEqualToBatch :: Point2d -> Property
prop_checkInRadiusEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkInRadiusEqualToBatch pointAsList2d distSqr2d (xs, radius, query)

prop_checkInRangeEqualToBatch :: ([Point2d], Point2d, Point2d) -> Bool
prop_checkInRangeEqualToBatch ([], _, _) = True
prop_checkInRangeEqualToBatch (xs, lowers, uppers)
  | and $ zipWith (<) (pointAsList2d lowers) (pointAsList2d uppers) =
      let kdt = KDM.buildWithDist pointAsList2d distSqr2d $ testElements xs
          kdtAnswer = KDM.inRange kdt lowers uppers
          dkdt = batchInsert (emptyWithDist pointAsList2d distSqr2d) $ testElements xs
          dkdtAnswer = inRange dkdt lowers uppers
      in  sort dkdtAnswer == sort kdtAnswer
  | otherwise = True


-- Run all tests
return []
runTests :: IO Bool
runTests = $(forAllProperties) $
  -- Vastly increase success counts; finds more bugs and our properties are cheap.
  quickCheckWithResult stdArgs{ maxSuccess = 2000 }

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
