module Robot (Robot, mkRobot, resetName, robotName) where

import System.Random
import Data.IORef
import Control.Monad

type Robot = IORef String

newName :: IO String
newName = (++) <$> replicateM 2 (randomRIO ('A', 'Z')) <*> replicateM 3 (randomRIO ('0', '9'))

mkRobot :: IO Robot
mkRobot = newName >>= newIORef

resetName :: Robot -> IO ()
resetName r = modifyIORef r . const =<< newName

robotName :: Robot -> IO String
robotName = readIORef