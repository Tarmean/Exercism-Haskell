module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify i
  | i <= 0 = Nothing
  | i == aliquotSum = Just Perfect
  | i < aliquotSum = Just Abundant
  | aliquotSum < i = Just Deficient
  where
    aliquotSum = sum $ filter isFactor [1 .. i `div` 2]
    isFactor j = i `mod` j == 0
