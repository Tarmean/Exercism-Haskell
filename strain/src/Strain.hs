module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep f = foldr (\i -> if f i then (i:) else id) []

discard = keep . (not.)