module Alphametics (solve) where
import Data.List (permutations, nub, foldl')
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String(Parser)
import Control.Monad (guard)

data Equation = Eq { lhs :: [String], rhs :: String } deriving Show
parseEq :: Parser Equation
parseEq = do lhs <- parse_lhs
             string "==" >> spaces
             rhs <- word
             return $ Eq { lhs = lhs, rhs = rhs }
    
parse_lhs = sepBy1 word (string "+" >> spaces)
word = do result <- many1 letter
          spaces
          return result


solve :: String -> Maybe [(Char, Int)]
solve input = headMaybe $ go eq [] uniqueLetters
  where eq = fromRight $ parse parseEq "" input
        uniqueLetters = nub $ concat (lhs eq) ++ (rhs eq)

go :: Equation -> [(Char, Int)] -> [Char] -> [[(Char, Int)]]
go equation mappings (l:letters) = do i <- [0..9]
                                      checkFirstNotZero equation l i
                                      checkNotInMappings mappings i
                                      let mappings' = (l, i) : mappings 
                                      go equation mappings' letters
go equation mappings []
  | isValid equation mappings = [mappings]
  | otherwise                 = []

checkFirstNotZero eq l i = guard $ notZero || notFirst
  where notZero = i /= 0
        notFirst = notFirstLhs && notFirstRhs
        notFirstLhs = all ((/= l) . head) $ lhs eq
        notFirstRhs = (/= l) . head $ rhs eq
checkNotInMappings mappings i = guard $ all ((/= i) . snd) mappings

isValid :: Equation -> [(Char, Int)] -> Bool
isValid eq mappings = left == right
  where left = sum . map wordValue $ lhs eq
        right = wordValue $ rhs eq

        wordValue word = foldl' step 0 word
        step acc cur = find cur + 10 * acc


        find l = fromJust $ lookup l mappings

fromJust (Just v) = v
fromRight (Right v) = v
headMaybe (x:xs) = Just x
headMaybe _      = Nothing
