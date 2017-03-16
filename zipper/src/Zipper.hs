module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Prelude hiding (Left, Right)
import Data.Maybe (fromJust)
type Child a = Maybe (BinTree a)
data BinTree a = BT { btValue :: a
                    , btLeft  :: Child a
                    , btRight :: Child a
                    } deriving (Eq, Show)

data Breadcrumb a = Left a (Child a) | Right a (Child a) deriving (Eq, Show)
data Zipper a = Z (BinTree a) [Breadcrumb a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Z  t []

toTree :: Zipper a -> BinTree a
toTree z@(Z t (b:bs)) =  toTree $ fromJust $ up z
toTree (Z t []) = t

value :: Zipper a -> a
value (Z (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Z (BT v (Just l) r) ls) = Just $ Z l $ (Left v r):ls
left _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Z (BT v l (Just r)) ls) = Just $ Z r $ (Right v l):ls
right _ = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Z l []) = Nothing
up (Z l ((Left v r):ls)) = Just $ Z (BT v (Just l) r) ls
up (Z r ((Right v l):ls)) = Just $ Z (BT v l (Just r)) ls

setValue :: a -> Zipper a -> Zipper a
setValue v (Z (BT _ l r) ls) = Z (BT v l r) ls

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Z (BT v _ r) ls) = Z (BT v l r) ls

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Z (BT v l _) ls) = Z (BT v l r) ls
