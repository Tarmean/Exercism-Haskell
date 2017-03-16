module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

-- The task is to create the data type `LinkedList`
-- and implement the functions below.
data LinkedList a = Cons a (LinkedList a) | Nil

datum :: LinkedList a -> a
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new v l = Cons v l

next :: LinkedList a -> LinkedList a
next (Cons a l) = l

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
