import Control.Monad (liftM2)
import Data.Foldable(fold)
import Data.Maybe (fromMaybe)

main = mapM_ putStrLn $ fizzbuzz [1..100]
fizzbuzz = build rules
build = map . liftM2 fromMaybe show . fold
rules = [rule 3 "Pling", rule 5 "Plang", rule 7 "Plong"]
  where rule i s j = if j `mod` i == 0 then Just s else Nothing
