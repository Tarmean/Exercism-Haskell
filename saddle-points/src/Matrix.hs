module Matrix (saddlePoints) where

import Data.Array

saddlePoints m = do
                   items <- assocs m
                   let (x, y) = fst items
                   let i = snd items
                   if i == minRow !! x &&
                      i == maxCol !! y
                   then return (x, y)
                   else []
  where maxCol = map minimum $ cols m
        minRow = map maximum $ rows m

rows = splitMatrix fst
cols = splitMatrix snd
splitMatrix toPos matrix = withIndex <$> [0..toPos size]
  where withIndex idx = map snd . filter ((==idx) . toPos . fst) $ assocs matrix
        size = snd $ bounds matrix
