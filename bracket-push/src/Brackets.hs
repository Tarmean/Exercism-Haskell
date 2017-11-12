module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go [] . filter (`elem` "[({])}")
  where
    go [] [] = True
    go rs ('[':xs) = go (']':rs) xs
    go rs ('(':xs) = go (')':rs) xs
    go rs ('{':xs) = go ('}':rs) xs
    go (r:rs) (c:xs)
      | c == r = go rs xs
    go _ _ = False
