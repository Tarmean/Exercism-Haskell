module Sieve (primesUpTo) where

primesUpTo i = go [2..i]
  where go [] = []
        go (x:ls) = x:go (x `withoutMultiples` ls)
        withoutMultiples i = filter ((/=0) . (`rem` i))