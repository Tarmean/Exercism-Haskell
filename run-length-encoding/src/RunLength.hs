{-# Language ViewPatterns #-}
module RunLength (decode, encode) where
import Data.Char
import Data.List
import Control.Monad.State
import Control.Monad.Loops

encode :: String -> String
encode = concatMap format . group
  where format = (++) <$> count <*> (return . head)
        count (length -> 1) = ""
        count (length -> len) = show len

decode :: String -> String
decode = concat . evalState (whileM notDone step)
  where step = replicate <$> times <*> first
        notDone = not . null <$> get
        times = readNum <$> state (span isDigit)
          where readNum "" = 1
                readNum n = read n
        first = state splitFirst
          where splitFirst (x:xs) = (x, xs)
