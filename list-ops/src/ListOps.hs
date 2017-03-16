module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc (x:xs) = foldr f (f x acc) xs
foldr _ acc [] = acc

length :: [a] -> Int
length [] = 0
length (x:xs)= 1 + length xs 

reverse :: [a] -> [a]
reverse [] =  []
reverse (x:xs) = r [x] xs
 where
   r s [] = s
   r s (x:xs) = r (x:s) xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) =  f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) =  if f x then x : filter f xs else filter f xs

(++) :: [a] -> [a] -> [a]
a ++ b = con (reverse a) b
  where 
    con [] ys = ys
    con (x:xs) ys =  xs ++ (x : ys)

concat :: [[a]] -> [a]
concat [] = []
concat (l:ls) = l ++ concat ls 
