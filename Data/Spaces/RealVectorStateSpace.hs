module Data.Spaces.RealVectorStateSpace 
    ( makeRealVectorStateSpace )

where

import qualified Data.FixedList as FL
import Control.Applicative
import Data.Foldable (sum)
import qualified Data.StateSpace as SS
import qualified Control.Monad.Random as CMR
import Data.Traversable

addV :: (FL.FixedList f) => f Double -> f Double -> f Double
v1 `addV` v2 = pure (+) <*> v1 <*> v2

minusV :: (FL.FixedList f) => f Double -> f Double -> f Double
v1 `minusV` v2 = pure (-) <*> v1 <*> v2

scaleV :: (FL.FixedList f) => f Double -> Double -> f Double
scaleV v a = fmap (*a) v

dotV :: (FL.FixedList f) => f Double -> f Double -> Double
v1 `dotV` v2 = Data.Foldable.sum $ pure (*) <*> v1 <*> v2

lengthV :: (FL.FixedList f) => f Double -> Double
lengthV v = sqrt $ v `dotV` v

normalizeV :: (FL.FixedList f) => f Double -> f Double
normalizeV v = let scale = 1.0 / (lengthV v)
               in  scaleV v scale

stateDistance :: (FL.FixedList f) => f Double -> f Double -> Double
stateDistance s1 s2 = lengthV $ s2 `minusV` s1

stateDistanceSqrd :: (FL.FixedList f) => f Double -> f Double -> Double
stateDistanceSqrd = dotV

interpolate :: (FL.FixedList f) => f Double -> f Double -> Double -> f Double
interpolate s1 s2 d = let vn = normalizeV $ s2 `minusV` s1
                      in  s1 `addV` (scaleV vn d)

getUniformSampler :: (FL.FixedList f, CMR.RandomGen g) => 
                     f Double -> f Double -> CMR.Rand g (f Double)
getUniformSampler minBound maxBound = let bounds = pure (,) <*> minBound <*> maxBound
                                      in  sequenceA $ fmap CMR.getRandomR bounds

makeRealVectorStateSpace :: (FL.FixedList f, CMR.RandomGen g) =>
                            f Double -> f Double -> SS.StateSpace (f Double) g
makeRealVectorStateSpace minBound maxBound = SS.StateSpace
                                             stateDistance
                                             stateDistanceSqrd
                                             interpolate
                                             (getUniformSampler minBound maxBound)
