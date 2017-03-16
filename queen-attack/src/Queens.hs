module Queens (boardString, canAttack) where
import Data.List

-- Positions are specified as two-tuples.
-- The first element is the row (rank in Chess terms).
-- The second element is is the column (file in Chess terms).
-- (0, 0) is the top left of the board, (0, 7) is the upper right,
-- (7, 0) is the bottom left, and (7, 7) is the bottom right.

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ line <$> [0..7]
 where
    line = unwords . flip fmap [0..7] . entry
    entry line column
      | current == white = "W" 
      | current == black = "B"
      | otherwise        = "_"
      where current = Just (line, column)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) =    x1 == x2
                              || y1 == y2
                              || abs (x1 - x2) == abs (y1 - y2)