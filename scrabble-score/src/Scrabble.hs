module Scrabble (scoreLetter, scoreWord) where
import Data.Monoid
import Data.Char


scoreWord :: String -> Int
scoreWord = getSum . foldMap (Sum . scoreLetter)

-- I guess creating this out of the readme via vim instead of finding an abstraction might count as cheating? Might have to revisit this.
scoreLetter c
  | toLower c `elem` "aeioulnrst" = 1
  | toLower c `elem` "dg"         = 2
  | toLower c `elem` "bcmp"       = 3
  | toLower c `elem` "fhvwy"      = 4
  | toLower c `elem` "k"          = 5
  | toLower c `elem` "jx"         = 8
  | toLower c `elem` "qz"         = 10
  | otherwise                     = 0
