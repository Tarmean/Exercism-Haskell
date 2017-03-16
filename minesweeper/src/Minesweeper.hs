module Minesweeper (annotate) where
import Data.List

annotate :: [String] -> [String]
annotate m = foldl' updateMap m $ findStars m

updateMap mat (i, j) = processLine <$>  zip [0..] mat
  where
    processLine (x, line) = [translate x y c| (y, c) <- zip [0..] line]
    translate x y c = if inRange x y 
                      then next c
                      else c
    inRange x y = abs (x - i) <=1 && abs (y - j) <=1

    next '*' = '*'
    next ' ' = '1'
    next c = toEnum $ 1 + fromEnum c
           
findStars :: [[Char]] -> [(Int, Int)]
findStars ls = do
  (x, line) <- zip [0..] ls
  (y, c) <- zip [0..] line
  if c == '*' then
    return (x, y)
  else []
