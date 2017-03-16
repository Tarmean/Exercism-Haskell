module Base (rebase) where

import Data.List (unfoldr)
import Data.Tuple (swap)
import Control.Monad (foldM)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 || outputBase < 2 = Nothing
  | otherwise = fromDec outputBase <$> toDec inputBase inputDigits

toDec base = foldM step  0
  where step acc i
         | i < 0 || i >= base = Nothing
         | otherwise          = Just $ base * acc + i
                                   
fromDec _ 0 = [0]
fromDec base ls = reverse . unfoldr step $ ls
  where 
    step 0    = Nothing
    step rest = Just . swap $ quotRem rest base
