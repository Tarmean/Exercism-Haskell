module Triangle (rows) where

rows :: Int -> [[Integer]]
rows i = take i $ iterate next [1]
  where
    next ls = 1 : (sums ls) ++ [1]
    sums = zipWith (+) <*> tail
