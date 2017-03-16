-- {-# LANGUAGE FlexibleInstances #-}
module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , Matrix.fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , Matrix.transpose
    ) where

import Data.String (IsString, fromString)
import Control.Monad
import Data.Vector as V
import qualified Data.List as L
-- import Control.Monad.State

data Matrix a = Matrix {getVec :: Vector (Vector a)} deriving (Show, Eq)

-- Implementation of a row-major matrix for any type, using Data.Vector.
--
-- No validation of input is required. Let it fail if the matrix is not
-- rectangular, invalid chars are encountered, etc.
--
-- shape is (rows, cols)
--
-- The task is to create the data type `Matrix`, with `Eq`
-- and `Show` instances, and implement the functions below.

cols :: Matrix a -> Int
cols m = if V.length v == 0 then 0 else V.length $ V.head v 
  where v = getVec m

column :: Int -> Matrix a -> Vector a
column i m = V.fromList $ V.foldr ((:).(!i)) [] (getVec m)

flatten :: Matrix a -> Vector a
flatten = V.concat . toList . getVec

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . L.map V.fromList


reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (i, j) = Matrix . V.unfoldr split . flatten
  where split = mfilter (not. V.null .fst) . Just . (V.splitAt i)

row :: Int -> Matrix a -> Vector a
row i m = getVec m ! i

rows :: Matrix a -> Int
rows = V.length . getVec

shape :: Matrix a -> (Int, Int)
shape = liftM2 (,) rows cols

transpose :: Matrix a -> Matrix a
transpose m = Matrix $ V.fromList [column i m | i <- [0..cols m-1]]

instance Read a => IsString (Matrix a) where
    fromString = Matrix.fromList . L.map parse . lines
parse s
   | L.null split = []
   | otherwise = entry: parse rest
   where
     split = reads s
     [(entry, rest)] = split
