module Change (findFewestCoins) where
changes = [100, 50, 20, 20, 5, 2]
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = undefined --go target choices []
  where
    choices = reverse coins

go :: [Int]-> Int -> [Int] -> [Int]
go current result choices = 
  where step choice
          | rest > 0
          | rest == 0
          | rest < 0
choices
