module Luhn (addends, checkDigit, checksum, create, isValid) where
import Data.Char (digitToInt)

modify i
  | n >= 10   = n - 9
  | otherwise = n
  where n = i*2

addends s = reverse $ zipWith ($) funcs ls 
  where 
    ls = reverse $ map digitToInt $ show s
    funcs = cycle [id, modify]

checkDigit = flip mod 10
checksum = checkDigit . sum . addends
create i =  (10-checksum n) `mod` 10 + n
  where n = 10 * i
isValid = (== 0) . checksum
