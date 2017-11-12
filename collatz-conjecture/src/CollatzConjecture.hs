module CollatzConjecture (collatz) where
import Data.List (genericLength)
import Control.Monad ((mfilter)

collatz :: Integer -> Maybe Integer
collatz =  fmap solve . mfilter (>0) . Just
  where
    solve = genericLength . takeWhile (/= 1) . iterate step
    step i
      | even i = i `div` 2
      | otherwise = 3 * i + 1
