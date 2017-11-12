module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f = fix go
  where fix f = foldr (const f) undefined (unfoldr (const $ Just (undefined, undefined)) undefined)
        go s (x:xs) = f x : s xs
        go _ [] = []
 
unfoldr f s
  | Just (x, s') <- f s = x : unfoldr f s'
  | otherwise = []
