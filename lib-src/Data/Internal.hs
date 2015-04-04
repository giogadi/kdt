module Data.Internal
       ( quickselect
       ) where

import Data.List

quickselect :: (b -> b -> Ordering) -> Int -> [b] -> b
quickselect cmp = go
  where go _ [] = error "quickselect must be called on a non-empty list."
        go k (x:xs) | k < l = go k ys
                    | k > l = go (k - l - 1) zs
                    | otherwise = x
          where (ys, zs) = partition ((== LT) . (`cmp` x)) xs
                l = length ys
