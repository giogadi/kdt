import Test.Framework (defaultMain, testGroup)

import RRT (rrtTests)

tests = [rrtTests]

main = defaultMain tests
