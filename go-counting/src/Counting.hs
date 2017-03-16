module Counting (
    Color(..),
    territories,
    territoryFor
) where
import Prelude as P
import Data.Set as S
import Data.Set

data Color = Black | White deriving (Eq, Ord, Show)
type Size = (Int, Int)
type Coord = (Int, Int)
type Board = [String]

territories :: Board -> [(Set Coord, Maybe Color)]
territories board = goAll empty positions
  where 
    size = getSize board
    positions = [(x, y) | x <- [1..fst size], y <- [1..snd size]]

    goAll seen [] = []
    goAll seen (present:future)
      | isEmpty && notSeen = (curSeen, curColor) : goAll seen' future
      | otherwise          =                       goAll seen  future 
      where 
        isEmpty = getEntry present board == ' '
        notSeen = not $ present `member` seen
        (curSeen, curColor) = traverseArea board size present
        seen' = curSeen `union` seen

territoryFor :: Board -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board (x, y)
  | outOfBounds  = Nothing
  | S.null coord = Nothing
  | otherwise    = Just (coord, color)
  where 
    (coord, color) = traverseArea board size (x, y)
    size = getSize board
    outOfBounds = x <= 0 || x > fst size ||
                  y <= 0 || y > snd size

traverseArea :: Board -> Size -> Coord -> (Set Coord, Maybe Color)
traverseArea board size start = go empty empty [start]
  where
    setToMaybe :: Set Color -> Maybe Color
    setToMaybe color
      | S.size color /= 1 = Nothing
      | otherwise = Just $ elemAt 0 color

    go :: (Set Color) -> (Set Coord) -> [Coord] -> (Set Coord, Maybe Color)
    go adjacent past [] =  (past, setToMaybe adjacent)
    go adjacent past (present:future)
      | seen      = go adjacent  past  future
      | notEmpty  = go adjacent' past  future
      | otherwise = go adjacent  past' future'
      where 
        seen     = present `member` past
        notEmpty = current /= ' '

        current   = present `getEntry` board
        adjacent' = current `insertColor` adjacent

        future' = future ++ neighbors present
        past'   = insert present past
            
        insertColor 'B' b = insert Black b
        insertColor 'W' b = insert White b
        insertColor  _  b = b

        neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], abs dx /= abs dy, onBoard (x+dx, y+dy)]
        onBoard (x, y) = x > 0 && x <= (fst size) && 
                         y > 0 && y <= (snd size)

getEntry (x, y) board = board !! (y-1) !! (x-1)
getSize board = (length $ head board, length board)
