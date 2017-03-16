{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bowling (BowlingError(..), score) where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import Data.List (find)

data BowlingError = IncompleteGame | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


data ParseState = ParserState [Int] Int
type BErr = Either BowlingError
newtype Parser i = Parser { runParser :: StateT ParseState BErr i } deriving (Functor, Applicative, Monad)
deriving instance MonadState ParseState Parser
deriving instance MonadError BowlingError Parser
instance Alternative Parser where
  m <|> n =  m `catchError` const n
  empty = Parser { runParser = StateT $ \_ -> throwError IncompleteGame }

score = parse game

parse :: Parser Int -> [Int] -> Either BowlingError Int
parse p v = do
  parsed <- runStateT (runParser p) $ ParserState v 0
  case parsed of
    (result, ParserState [] idx) -> return result
    (_, ParserState (x:_) idx) -> throwError InvalidRoll {rollIndex = idx, rollValue = x }

game :: Parser Int
game = do
  beginning <- replicateM 9 $ frame False
  ending <- frame True
  return $ sum beginning + ending
frame ::  Bool -> Parser Int
frame consume = strike consume <|> do
  first <- remaining 0
  second <- remaining first
  if first + second == 10
  then do
    bonus <- bonus consume 1
    return $ first + second + bonus
  else return $ first + second
strike ::  Bool -> Parser Int
strike consume = do
  satisfy (==10)
  boni <- bonus consume 2
  return $ 10 + boni

bonus ::  Bool -> Int -> Parser Int
bonus consume 2 = f $ do
  a <- remaining 0
  b <- remaining a
  return $ a + b
  where f = if consume then id else lookAhead
bonus consume 1 = f $ remaining 0
  where f = if consume then id else lookAhead

lookAhead :: Parser a -> Parser a
lookAhead t = do
  state <- get
  result <- t
  put state
  return result

remaining 10 = satisfy $ liftA2 (&&) (>=0) (<=10)
remaining i = satisfy $ liftA2 (&&) (>=0) (<=10-i)

satisfy :: (Int->Bool)  -> Parser Int
satisfy cond = do
  ParserState source pos <- get
  case source of
    (x:xs) ->
      if cond x
      then do
        put $ ParserState xs (pos+1)
        return x
      else throwError InvalidRoll {rollIndex = pos, rollValue = x}
    _ -> throwError IncompleteGame
