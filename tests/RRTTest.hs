import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2

import RRT

tests = [testGroup "RRT primitives group" [
                        testProperty "Nonnegative distance" prop_nonnegDist,
                        testProperty "Squared distance" prop_squaredDist,
                        testProperty "Extend limit" prop_extendLimit]]

main = defaultMain tests
