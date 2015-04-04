{-# LANGUAGE TemplateHaskell, CPP #-}

import Control.Monad
import Data.List
import Data.Ord
import System.Exit
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

import Data.VPMap.Static as VPM
import Data.Point2d

import Debug.Trace

testElements :: [Point2d] -> [(Point2d, Int)]
testElements ps = zip ps [0 ..]

prop_validTree :: Property
prop_validTree =
  forAll (listOf1 arbitrary) $
    isValid . build dist2d . testElements

checkElements :: [Point2d] -> Bool
checkElements ps =
  let vpm = build dist2d $ testElements ps
  in  sort (assocs vpm) == sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements

checkNumElements :: [Point2d] -> Bool
checkNumElements ps =
  let vpm = build dist2d $ testElements ps
  in  VPM.size vpm == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements

nearestLinear :: [(Point2d, v)] -> Point2d -> (Point2d, v)
nearestLinear xs query =
  minimumBy (comparing (dist2d query . fst)) xs

checkNearestEqualToLinear :: ([Point2d], Point2d) -> Bool
checkNearestEqualToLinear (ps, query) =
  let vpm = build dist2d $ testElements ps
      vpmNearest = nearest vpm query
      linearNearest = nearestLinear (testElements ps) query
  in  vpmNearest == linearNearest

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear (xs, query)

inRadiusLinear :: [(Point2d, v)] -> Point2d -> Double -> [(Point2d, v)]
inRadiusLinear xs query radius =
  filter ((<= radius) . dist2d query . fst) xs

checkInRadiusEqualToLinear :: Double -> ([Point2d], Point2d) -> Bool
checkInRadiusEqualToLinear radius (ps, query) =
  let vpm = build dist2d $ testElements ps
      vpmNear = inRadius vpm radius query
      linearNear = inRadiusLinear (testElements ps) query radius
  in  sort vpmNear == sort linearNear

prop_inRadiusEqualToLinear :: Point2d -> Property
prop_inRadiusEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkInRadiusEqualToLinear radius (xs, query)

kNearestLinear :: [(Point2d, v)] -> Point2d -> Int -> [(Point2d, v)]
kNearestLinear xs query k =
  take k $ sortBy (comparing (dist2d query . fst)) xs

checkKNearestEqualToLinear :: Int -> ([Point2d], Point2d) -> Bool
checkKNearestEqualToLinear k (xs, query) =
  let vpm = build dist2d $ testElements xs
      vpmKNear = kNearest vpm k query
      vpmDists = map (dist2d query . fst) vpmKNear
      linearKNear = kNearestLinear (testElements xs) query k
      linearDists = map (dist2d query . fst) linearKNear
  in  trace ("WRONG " ++ show vpmDists) vpmKNear == trace ("RIGHT " ++ show linearDists) linearKNear

prop_kNearestEqualToLinear :: Point2d -> Property
prop_kNearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToLinear k (xs, query)

checkKNearestSorted :: ([Point2d], Point2d) -> Bool
checkKNearestSorted ([], _) = True
checkKNearestSorted (ps, query) =
  let vpm = build dist2d $ testElements ps
      kNearestDists =
        map (dist2d query . fst) $ kNearest vpm (length ps) query
  in  kNearestDists == sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted (xs, query)

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
