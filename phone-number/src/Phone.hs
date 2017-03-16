module Phone (areaCode, number, prettyPrint) where
import Data.Char
import Text.Printf

areaCode  = fmap (take 3) . number

number = process .filter isNumber
  where process num
          | length num == 10                       = Just num
          | length num == 11 && (head num == '1')  = Just $ tail num
          | otherwise                              = Nothing

prettyPrint s = format <$> sub 0 3  <*> sub 3 3 <*> sub 6 4
  where 
    sub l r = fmap (take r . drop l) $ number s

    format :: String -> String -> String -> String
    format = printf "(%s) %s-%s"