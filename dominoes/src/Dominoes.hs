module Dominoes (chain) where
import Data.List

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain []           = Just []
chain (first:rest) = headMaybe $ go rest [first]

go [] path
  | left == right = return path
  | otherwise     = []
  where (left, _)  = head path
        (_, right) = last path
go stones path = do ((a, b), rest) <- choose stones
                    let (l, _) = head path
                    if a == l then 
                      go rest ((b, a): path)
                    else if b == l then
                      go rest ((a, b): path)
                      else []



headMaybe (x:xs) = Just x
headMaybe _      = Nothing
choose ls = zip ls $ zipWith (++) (inits ls) (tail $ tails ls)

