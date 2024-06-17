{-# LANGUAGE TemplateHaskell, CPP #-}

import Data.KdMap.Static as KDM

import Control.Monad (unless)
import Data.List (null, sort, sortBy)
import qualified Data.Set as Set
import Data.Set (isSubsetOf)
import Data.Point2d
import System.Exit (exitFailure)
import Test.QuickCheck (Property, arbitrary, forAllProperties, forAll, listOf1, choose, Args(..), stdArgs, quickCheckWithResult)

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

import Tests.TestHelpers (nearestsLinear, withinDistanceOfKthNearest)

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [0 ..]

prop_validTree :: Property
prop_validTree =
  forAll (listOf1 arbitrary) $ isValid . build pointAsList2d . testElements

checkElements :: (Ord p, Real a) => PointAsListFn a p -> [p] -> Bool
checkElements pointAsList ps =
  let kdt = build pointAsList $ testElements ps
  in  sort (assocs kdt) == sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements pointAsList2d

checkNumElements :: Real a => PointAsListFn a p -> [p] -> Bool
checkNumElements pointAsList ps =
  let kdm = build pointAsList $ testElements ps
  in  size kdm == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements pointAsList2d

checkNearestConsistentWithLinear :: (Eq p, Real a, Show p) => KDM.PointAsListFn a p -> ([p], p) -> Bool
checkNearestConsistentWithLinear pointAsList (ps, query) =
  let kdt = build pointAsList $ testElements ps
  in  nearest kdt query `elem` nearestsLinear pointAsList (testElements ps) query

prop_nearestConsistentWithLinear :: Point2d -> Property
prop_nearestConsistentWithLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestConsistentWithLinear pointAsList2d (xs, query)

inRadiusLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> a -> [(p, v)]
inRadiusLinear pointAsList xs query radius =
  filter ((<= radius * radius) . defaultSqrDist pointAsList query . fst) xs

checkInRadiusEqualToLinear :: (Ord p, Real a) => KDM.PointAsListFn a p -> a -> ([p], p) -> Bool
checkInRadiusEqualToLinear pointAsList radius (ps, query) =
  let kdt = build pointAsList $ testElements ps
      kdtNear = inRadius kdt radius query
      linearNear = inRadiusLinear pointAsList (testElements ps) query radius
  in  sort kdtNear == sort linearNear

prop_inRadiusEqualToLinear :: Point2d -> Property
prop_inRadiusEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkInRadiusEqualToLinear pointAsList2d radius (xs, query)

checkKNearestConsistentWithLinear :: (Ord p, Real a) => KDM.PointAsListFn a p -> Int -> ([p], p) -> Bool
checkKNearestConsistentWithLinear pointAsList k (xs, query) =
  let kdt = build pointAsList $ testElements xs
      kdtKNear = kNearest kdt k query
      possibleNearest = withinDistanceOfKthNearest pointAsList (testElements xs) query k
  in  Set.fromList kdtKNear `isSubsetOf` Set.fromList possibleNearest

prop_kNearestConsistentWithLinear :: Point2d -> Property
prop_kNearestConsistentWithLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestConsistentWithLinear pointAsList2d k (xs, query)

checkKNearestSorted :: (Eq p, Real a) => KDM.PointAsListFn a p -> ([p], p) -> Bool
checkKNearestSorted _ ([], _) = True
checkKNearestSorted pointAsList (ps, query) =
  let kdt = build pointAsList $ testElements ps
      kNearestDists =
        map (defaultSqrDist pointAsList query . fst) $ kNearest kdt (length ps) query
  in  kNearestDists == sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted pointAsList2d (xs, query)

rangeLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> p -> [(p, v)]
rangeLinear pointAsList xs lowers uppers =
  let valInRange a lower upper = lower <= a && a <= upper
      lowersAsList = pointAsList lowers
      uppersAsList = pointAsList uppers
      pointInRange (p, _) =
        and $ zipWith3 valInRange (pointAsList p) lowersAsList uppersAsList
  in  filter pointInRange xs

prop_rangeEqualToLinear :: ([Point2d], Point2d, Point2d) -> Bool
prop_rangeEqualToLinear (xs, lowers, uppers)
  | Data.List.null xs = True
  | and $ zipWith (<) (pointAsList2d lowers) (pointAsList2d uppers) =
      let linear = rangeLinear pointAsList2d (testElements xs) lowers uppers
          kdt    = build pointAsList2d $ testElements xs
          kdtPoints = inRange kdt lowers uppers
      in  sort linear == sort kdtPoints
  | otherwise = True

prop_equalAxisValueSameElems :: Property
prop_equalAxisValueSameElems =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkElements pointAsList2d $ Point2d x (y + 1) : xs

prop_equalAxisValueEqualToLinear :: Point2d -> Property
prop_equalAxisValueEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkNearestConsistentWithLinear pointAsList2d (Point2d x (y + 1) : xs, query)

prop_unbalancedInsertValid :: Property
prop_unbalancedInsertValid =
  forAll (listOf1 arbitrary) $
    isValid . batchInsertUnbalanced (empty pointAsList2d) . testElements

prop_unbalancedInsertNNConsistentWithLinear :: Point2d -> Property
prop_unbalancedInsertNNConsistentWithLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    let kdm = batchInsertUnbalanced (empty pointAsList2d) $ testElements xs
    in  nearest kdm query `elem` nearestsLinear pointAsList2d (testElements xs) query

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
