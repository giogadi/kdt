module Data.DiscreteMotionValidator
       ( motionValid ) where

import qualified Data.StateSpace as SS

motionValid :: SS.StateSpace s g -> (s -> Bool) -> Double -> s -> s -> Bool
motionValid ss f h s1 s2
  | h <= 0.0  = error "Data.DiscreteMotionValidator.motionValid must have a positive step size"
  | otherwise = let n = floor $ ((SS._stateDistance ss) s1 s2) / h :: Int
                    samplePts = scanl1 (+) (replicate n h)
                    innerValid = all f $ map ((SS._interpolate ss) s1 s2) samplePts
                in  innerValid && (f s2)
