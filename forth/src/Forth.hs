{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , Forth.empty
  ) where

import Control.Monad.State
import Control.Monad (foldM, liftM2)
import Data.Char (isSpace, isControl, toLower, isDigit, toUpper)
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text(Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Map (Map, member, (!), insert)
import qualified Data.Map as M

data Token = Definition String [Token] | Builtin String | Op String | Num Int deriving (Eq, Show)
type DefinitionMap = Map String [Token]
type Stack = [Int]
type ForthState = (DefinitionMap, Stack) --State DefinitionMap [Int]
data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)
empty :: ForthState
empty = (M.empty, [])

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText t s = case tokenize t of Right tokens -> foldM step s tokens
                                  _            -> Left InvalidWord 

step :: ForthState -> Token -> Either ForthError ForthState
step (defs, stack) (Num i)     = Right (defs, i:stack)
step (defs, stack) (Definition ident def) = Right (defs', stack)
  where defs' = insert ident def defs
step s@(defs, stack) (Op op)
  | op `member` defs = foldM step s (defs ! op)
  | otherwise = Left . UnknownWord $ pack op
step s@(defs, stack) (Builtin op)
  | op `member` defs = foldM step s (defs ! op)
  | otherwise = builtIn op s

builtIn :: String -> ForthState -> Either ForthError ForthState
builtIn "dup" (defs, (x:rest)) = Right $ (defs, x:x:rest)
builtIn "drop" (defs, (x:rest)) = Right $ (defs, rest)
builtIn op (defs, w@(x:y:rest)) = process op
  where process "+" = applyOp (+)
        process "-" = applyOp (-)
        process "*" = applyOp (*)
        process "/" = if x /= 0
                      then applyOp div
                      else Left DivisionByZero
        process "swap" = r $ y:x:rest
        process "over" = r $ y:x:y:rest
        process _ = Left . UnknownWord $ pack op
        applyOp f = r $ (f y x) : rest
        r stack' = Right (defs, stack')
builtIn _ s = Left StackUnderflow

formatStack :: ForthState -> Text
formatStack (def, state) = pack . unwords .reverse $ map show state




tokenize s = parse parseTokens "" s
parseTokens :: Parser [ Token ]
parseTokens = dividers >> (many $ (try builtin) <|> definition <|> number <|> op)
builtin = do b <- stringi "dup" <|> stringi "drop" <|> stringi "swap" <|> stringi "over" <|> 
                  string "+" <|> string "-" <|> string "*" <|> string "/" <?> "Builtin"
             dividers1 <|> (eof >> return "")
             return . Builtin . (map toLower) $ b
number = do num <- many1 digit
            dividers
            return . Num $ read num
definition = do _ <- char ':'
                dividers
                identifier <- word
                dividers
                definition <- parseTokens
                _ <- char ';'
                dividers
                return $ Definition identifier definition
op = do op <- word
        dividers
        return $ Op op
word = map toLower <$> many1 wordChar
  where wordChar = satisfy wPred
        wPred c =  c /= ';'  && (not $ isDigit c) && (not $ divPred c)

chari :: Char -> Parser Char
chari c = char (toLower c) <|> char (toUpper c)
stringi :: String -> Parser String
stringi s = try (mapM chari s) <?> "\"" ++ s ++ "\""
dividers = many $ satisfy divPred
dividers1 = many1 $ satisfy divPred
divPred c = c == ' ' || isSpace c || isControl c
