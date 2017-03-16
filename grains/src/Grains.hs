module Grains (square, total) where
import Data.Maybe
-- I forgot to import maybe. Whoops.
square x
   | 0 < x && x <= 64 = Just $ 2^( x-1 )
   | otherwise        = Nothing

total = sum $ mapMaybe square [1..64]
