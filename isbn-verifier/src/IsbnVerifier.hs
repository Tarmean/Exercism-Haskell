module IsbnVerifier (isbn) where

import Data.Char

isbn :: String -> Bool
isbn isbn
   | Just l <- start, Just r <- end, isbnCheck l r = True
   | otherwise = False
   where
       isbnCheck l r = length l == 9 && formula l r `mod` 11 == 0
       formula l r = sum (zipWith (*) l [10, 9..2]) + r

       start = traverse fromDigit $ filter (/= '-') (init isbn)
       end = checksumToInt (last isbn)


       fromDigit i
           | isDigit i = Just (digitToInt i)
           | otherwise = Nothing

       checksumToInt 'X' = Just 10
       checksumToInt i = fromDigit i
