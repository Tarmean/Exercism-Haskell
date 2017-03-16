{-# LANGUAGE Rank2Types #-}
module Connect (Mark(..), winner) where
data Mark = Cross | Nought deriving (Eq, Show)
type Board = [[Char]]

winner :: Board -> Maybe Mark
winner prettyBoard
  | hasWon fst = Just Cross
  | hasWon snd = Just Nought
  | otherwise = Nothing
  where
    hasWon :: (forall a . (a, a) -> a) -> Bool
    hasWon side = any isEndPoint reachable
      where teams = ('X', 'O') 
            start = [(x, y)| x <- [0..fst size - 1], y <- [0..snd size - 1], side (x, y) == 0]
            reachable = parse (side teams) [] start
            isEndPoint = (==(side size)-1) . side

    parse _ past [] = past
    parse team past (present:future)
      | inTeam present = parse team (present:past) (future ++ toDo)
      | otherwise      = parse team past future
      where possibilities = neighbors present
            toDo = filter (not.visited) possibilities 
            visited l = (l `elem` past) || (l ` elem` future)
            inTeam (x, y) = board !! y !! x == team

    neighbors (row, col) = [(row + x, col + y) | (x, l) <- offsets, y <- l, onBoard (row+x) (col+y)]
    offsets = zip [-1..1] [[0, 1], [-1, 1], [-1, 0]]
    onBoard x y = x >= 0 && x < (fst size) && y >= 0 && y < (snd size)

    size = (length $ head board, length board)
    board = map (filter (/= ' ')) prettyBoard
