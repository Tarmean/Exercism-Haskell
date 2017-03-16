module Prime (nth) where
import Control.Monad (join)
nth i
  | i > 0 = Just . head $ find 2 [] i
  | otherwise = Nothing
  where
    find _   found 0    = found
    find cur found left = if   isPrime
                          then find next (cur:found) (left-1)
                          else find next found       left
      where next = cur+1
            isPrime = all notDivisible possiblePrimes

            notDivisible :: Integer -> Bool
            notDivisible = (>0) . (cur `rem`)
            possiblePrimes = takeWhile lessThanSqrt found
            lessThanSqrt = (<= cur*2) 
