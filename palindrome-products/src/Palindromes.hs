module Palindromes (largestPalindrome, smallestPalindrome) where

isPalindrome i = (==) =<< reverse $ show i
palindromes head tail = do x <- [head .. tail]
                           y <- [x .. tail]
                           if isPalindrome (x*y)
                           then return (x, y)
                           else []

filteredPalindromes :: ([Integer] -> Integer) -> Integer -> Integer -> (Integer, [(Integer, Integer)])
filteredPalindromes f x y = (exPoint, exPairs)
  where
    pairs =  palindromes x y
    exPoint = f . map (uncurry (*)) $ pairs
    exPairs = filter ((==exPoint) . uncurry (*)) pairs

largestPalindrome  = filteredPalindromes maximum
smallestPalindrome = filteredPalindromes minimum
