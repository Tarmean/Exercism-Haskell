module CustomSet
  ( CustomSet
  , delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , CustomSet.null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import Control.Applicative (liftA2)
import Data.List (nub, sort, null)
import Data.Function (on) -- hoogle be blessed

-- The task is to create the data type `CustomSet`, with `Eq`
-- and `Show` instances, and implement the functions below.

newtype CustomSet a = CustomSet { getRep :: [a] } deriving (Show)



liftSet  = (`on` getRep)
liftToSet = ((CustomSet.).). liftSet

instance (Ord a) => Eq (CustomSet a) where
  (==) = liftSet ((==) `on` sort)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete = (CustomSet .) . (. getRep) . filter . (/=)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference =  liftToSet (flip $ filter . notIn)
  where notIn = (not .) . flip elem

empty :: CustomSet a
empty = CustomSet []

fromList :: Ord a => [a] -> CustomSet a
fromList = CustomSet . nub

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert =  (clean.) . (. getRep) . (:)
  where clean = CustomSet . nub

intersectLists :: Eq a => [a] -> [a] -> [a]
intersectLists = filter . flip elem

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection =  liftToSet intersectLists

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom = (Data.List.null.) . liftSet intersectLists

null :: CustomSet a -> Bool
null = Data.List.null . getRep

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf = liftSet (flip allIn)
  where allIn = all . flip elem

member :: Eq a => a -> CustomSet a -> Bool
member = (.getRep) . elem 

size :: CustomSet a -> Int
size = length . getRep

toList :: CustomSet a -> [a]
toList = getRep

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union = liftToSet ((nub.) . (++))
