module SumOfMultiples (sumOfMultiples) where
import Data.List


sumOfMultiples =  ((sum . nub ).) . multiples
  where
    multiples = (.steps) . (>>=)
    steps max = takeWhile (<max) . (iterate =<< (+))