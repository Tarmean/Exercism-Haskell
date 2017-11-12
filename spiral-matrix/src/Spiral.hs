module Spiral (spiral) where
import qualified Data.Vector.Mutable as VM
import Data.Vector.Mutable (STVector)
import Control.Lens
import Control.Monad.ST

spiral :: Int -> [[Int]]
spiral = undefined

squareVec :: Int -> ST s (STVector s (STVector s a))
squareVec size = VM.replicateM size (VM.new size)

-- f :: ST s a
-- f = do
--     s <- squareVec 5
--     ix 1 . ix 2
-- -- insert :: (MonadState MatrixState m) => m ()
-- -- insert = do
-- --     (x, y) <- getNext 
    
-- -- spiral size = do
-- --   where
-- --     step = do

