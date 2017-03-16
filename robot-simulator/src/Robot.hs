module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where
import Data.List (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot {
                     bearing :: Bearing,
                     coordinates :: (Integer, Integer)
                   } deriving (Eq, Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate  = foldl' step
  where
    step (Robot North (x, y)) 'A' = Robot North (x, y+1)
    step (Robot East  (x, y)) 'A' = Robot East  (x+1, y)
    step (Robot South (x, y)) 'A' = Robot South (x, y-1)
    step (Robot West  (x, y)) 'A' = Robot West  (x-1, y)
    step (Robot d     p)      'L' = Robot (turnLeft  d) p
    step (Robot d     p)      'R' = Robot (turnRight d) p

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft d = pred d

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight d    = succ d