module Hamming (distance) where


distance [] [] = Just 0
distance (x:xs) (y:ys) = (+) (if x /= y then 1 else 0) <$> distance xs ys
distance _ _ = Nothing