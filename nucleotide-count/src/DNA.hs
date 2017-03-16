module DNA (count, nucleotideCounts) where

import Data.Map as M
import Data.List as L

count c s = (!) <$> nucleotideCounts s <*> (toNucleotide c)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = L.foldr step zero
  where
    step c acc = M.adjust succ <$> toNucleotide c <*> acc
    zero = Right $ M.fromList $ L.map (flip (,) 0) "CTAG"

toNucleotide c
  | c `elem` "CTAG" = Right c
  | otherwise       = Left $ "Invalid Input: " ++ [c]
