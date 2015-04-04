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

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
