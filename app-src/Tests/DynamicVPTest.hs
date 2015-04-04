{-# LANGUAGE TemplateHaskell, CPP #-}

import qualified Data.VPMap.Static as VPM
import Data.VPMap.Dynamic

import Control.Monad
import Data.Bits
import Data.List
import Data.Point2d
import System.Exit
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

testElements :: [Point2d] -> [(Point2d, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: [Point2d] -> Bool
checkLogNTrees ps =
  let lengthIsLogN vpm = length (subtreeSizes vpm) == popCount (size vpm)
  in  all lengthIsLogN $ scanl insertPair (empty dist2d) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees

checkTreeSizesPowerOf2 :: [Point2d] -> Bool
checkTreeSizesPowerOf2 ps =
  let sizesPowerOf2 = all ((== 1) . popCount) . subtreeSizes
  in  all sizesPowerOf2 $ scanl insertPair (empty dist2d) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2

checkNumElements :: [Point2d] -> Bool
checkNumElements ps =
  let numsMatch (num, vpm) = size vpm == num && num == sum (subtreeSizes vpm)
  in  all numsMatch $ zip [0..] $
        scanl insertPair (empty dist2d) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements

checkNearestEqualToBatch :: ([Point2d], Point2d) -> Bool
checkNearestEqualToBatch (ps, query) =
  let vpm = VPM.build dist2d $ testElements ps
      vpmAnswer = VPM.nearest vpm query
      dvpm = batchInsert (empty dist2d) $ testElements ps
      dvpmAnswer = nearest dvpm query
  in  dvpmAnswer == vpmAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch (xs, query)

checkKNearestEqualToBatch :: ([Point2d], Int, Point2d) -> Bool
checkKNearestEqualToBatch (ps, k, query) =
  let vpm = VPM.build dist2d $ testElements ps
      vpmAnswer = VPM.kNearest vpm k query
      dvpm = batchInsert (empty dist2d) $ testElements ps
      dvpmAnswer = kNearest dvpm k query
  in  dvpmAnswer == vpmAnswer

prop_kNearestEqualToBatch :: Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch (xs, k, query)

checkInRadiusEqualToBatch :: ([Point2d], Double, Point2d) -> Bool
checkInRadiusEqualToBatch (ps, radius, query) =
  let vpm = VPM.build dist2d $ testElements ps
      vpmAnswer = VPM.inRadius vpm radius query
      dvpm = batchInsert (empty dist2d) $ testElements ps
      dvpmAnswer = inRadius dvpm radius query
  in  sort dvpmAnswer == sort vpmAnswer

prop_checkInRadiusEqualToBatch :: Point2d -> Property
prop_checkInRadiusEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkInRadiusEqualToBatch (xs, radius, query)

-- Run all tests
return []
runTests :: IO Bool
runTests =  $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
