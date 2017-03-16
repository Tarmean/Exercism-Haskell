module Deque (Deque, mkDeque, pop, push, shift, unshift) where

data Deque a = Cons [a] a [a]
-- The task is to create the data type `Deque`
-- and implement the functions below.

mkDeque :: IO (Deque a)
mkDeque = return Empty

pop :: Deque a -> IO (Maybe a)
pop (Cons l a r) = undefined

push :: Deque a -> a -> IO ()
push = undefined

unshift :: Deque a -> a -> IO ()
unshift = undefined

shift :: Deque a -> IO (Maybe a)
shift = undefined
