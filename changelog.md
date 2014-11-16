# 0.2.0
* Lots and lots of renaming all throughout to more closely match terminology used in `containers`.
* Removed kdt library dependency on QuickCheck (if not building testing code).
* Removed testing module Point2d from public API
* All structures now have Show instance
* Static variants now have functions for dynamically inserting new points into existing structure, with caveat that these functions do not maintain balanced tree structure.
