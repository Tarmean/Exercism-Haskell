module WordProblem (answer) where
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Data.List (foldl')

answer = unwrapped . result
  where
    unwrapped (Left _)  = Nothing
    unwrapped (Right r) = Just r

    result = parse parseInput "(undefined)"

parseInput = do
  string "What is" >> spaces
  start <- parseNum
  ops <- parsePairs
  string "?" >> eof
  -- this is kind of horrible but that is what you get for bringing me to use parsec to tokenize a regular grammar :P
  return $ foldl' (flip ($)) start ops 

parsePairs = many1 $ do
  op <- parseOp
  spaces
  num <- parseNum
  return $ flip op num

parseNum = do
  -- the tests pass and I am lazy
  num <- many1 $ digit <|> oneOf "(-)"
  spaces
  return $ read num

parseOp = tAdd <|> try tSub <|> tMult <|> tDiv <?> "Operator"
  where tAdd  = string "plus"  >> return (+)
        tSub  = string "minus"  >> return (-)
        tMult = string "divided by"  >> return div
        tDiv  = string "multiplied by"  >> return (*)