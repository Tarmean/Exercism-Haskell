module Sublist (Sublist(..), sublist) where
import Data.List (isInfixOf)

data Sublist = Equal|Sublist|Superlist|Unequal deriving (Show, Eq)

sublist a b
      |a == b        = Equal
      |isInfixOf a b = Sublist
      |isInfixOf b a = Superlist
      |otherwise     = Unequal

