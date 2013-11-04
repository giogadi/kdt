module Data.Spaces.Point2DSpace
    ( Point2D(..)
    , makePoint2DSpace ) where

import qualified Control.Monad.Random as CMR
import Control.Monad (liftM2)
import qualified Data.StateSpace as SS

data Point2D = Point2D !Double !Double

instance Show Point2D where
    show (Point2D x y) = show x ++ " " ++ show y

stateDistanceSqrd :: Point2D -> Point2D -> Double
stateDistanceSqrd (Point2D x1 y1) (Point2D x2 y2) = 
    let v1 = x2 - x1
        v2 = y2 - y1
    in  v1*v1 + v2*v2

stateDistance :: Point2D -> Point2D -> Double
stateDistance p1 p2 = sqrt $ stateDistanceSqrd p1 p2

interpolate :: Point2D -> Point2D -> Double -> Point2D
interpolate (Point2D x1 y1) (Point2D x2 y2) d =
    let v1 = x2 - x1
        v2 = y2 - y1
    in  Point2D (x1 + d*v1) (y1 + d*v2)

getUniformSampler :: (CMR.RandomGen g) => Point2D -> Point2D -> CMR.Rand g (Point2D)
getUniformSampler (Point2D xmin ymin) (Point2D xmax ymax) =
    (liftM2 Point2D) 
    (CMR.getRandomR (xmin, xmax))
    (CMR.getRandomR (ymin, ymax))

makePoint2DSpace :: CMR.RandomGen g => Point2D -> Point2D -> SS.StateSpace Point2D g
makePoint2DSpace pmin pmax = SS.StateSpace
                             stateDistance
                             stateDistanceSqrd
                             interpolate
                             (getUniformSampler pmin pmax)
