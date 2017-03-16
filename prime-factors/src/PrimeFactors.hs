module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors i = factors i 2 []
  where
    factors 1 _ ls = ls
    factors i c ls = if i `mod` c == 0
                     then c : factors (i `div` c) c (ls)
                     else factors i (c+1) ls