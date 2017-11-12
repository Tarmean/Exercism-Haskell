{-# Language FlexibleContexts #-}
module Brackets (arePaired) where
import Control.Monad.State
import Control.Monad.Catch
import Data.Typeable
import Data.Foldable
import Control.Monad

arePaired :: String -> Bool
arePaired = maybe False (const True)
    . mfilter (null . snd)
    . flip runStateT []
    . traverse_  step
    . filter (`elem` "[({})]")

step :: (MonadThrow m, MonadState String m) => Char -> m ()
step l = case lookup l brackets of
  Just r -> pop r
  Nothing -> push l
  where
    push x = modify (x:)
    pop y = do
      (x:xs) <- get
      if x /= y
      then throwM MismatchException
      else put xs
    brackets = zip "])}" "[({"

data MismatchException = MismatchException
    deriving (Show, Typeable)

instance Exception MismatchException
