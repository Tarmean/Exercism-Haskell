module Squares (difference, squareOfSums, sumOfSquares) where
import Control.Applicative

difference :: Integral a => a -> a
difference = liftA2 (-) squareOfSums sumOfSquares

squareOfSums :: Integral a => a -> a
squareOfSums n= (n*(n+1))^2 `div` 4

sumOfSquares :: Integral a => a -> a
sumOfSquares n =  n * (n+1) * (2*n+1) `div` 6