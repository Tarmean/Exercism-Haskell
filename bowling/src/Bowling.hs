{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
module Bowling (BowlingError(..), score) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Control.Monad


data BowlingError = IncompleteGame | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score throws = runParser pGame () "Game" throws

type ParserT m a = ParsecT [Int] () m a

pGame :: (Monad m) => ParserT m Int
pGame = do
  beginning <- replicateM 9 $ pFrame (lookAhead . pBonus)
  end <- pFrame pBonus
  eof
  return (sum beginning + end)
pFrame bonus = msum $ [pStrike, pSpare, pOpen] <*> pure bonus
pStrike bonus = liftM2 (+) (pThrow 10) (bonus 2)
pSpare bonus = try $ do
  x <- anyThrow
  y <- anyThrow
  guard (x + y == 10)
  bonus <- bonus 1
  return (x + y + bonus)
pOpen bonus = try $ do
  x <- anyThrow
  y <- anyThrow
  guard (x + y < 10)
  return (x + y)

pBonus :: (Monad m) => Int -> ParserT m Int
pBonus i = sum <$> (replicateM i anyThrow)

anyThrow :: (Monad m) => ParserT m Int
anyThrow = satisfy (const True)
pThrow :: (Monad m) => Int -> ParserT m Int
pThrow i = satisfy (==i)

satisfy :: (Stream s m Int) => (Int -> Bool) -> ParsecT s u m Int
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos _c _cs -> incSourceColumn pos 1)
                                (\c -> if f c then Just c else Nothing)
