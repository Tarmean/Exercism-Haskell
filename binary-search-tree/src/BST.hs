module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.List (foldl')
-- The task is to create the data type `BST`, with `Eq`
-- and `Show` instances, and implement the functions below.
data BST a = Cons a (BST a) (BST a) | Leaf deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Cons _ Leaf _) = Nothing
bstLeft (Cons _ l _) = Just l


bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Cons _ _ Leaf) = Nothing
bstRight (Cons _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Cons v _ _) = Just v

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert a Leaf = singleton a
insert a (Cons v l r)
   | a <= v    = Cons v (insert a l) r
   | otherwise = Cons v l (insert a r)

singleton :: a -> BST a
singleton a = Cons a Leaf Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Cons v l r) = toList l ++ [v] ++ toList r