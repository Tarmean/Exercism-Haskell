module Anagram (anagramsFor) where
import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor =  filter . testAnagram
  where
    testAnagram  = (. map toLower) . test .  map toLower
    test a b = ((sort a) == (sort b)) && (a /= b)