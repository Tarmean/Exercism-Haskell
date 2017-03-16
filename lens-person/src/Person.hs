{-# LANGUAGE TemplateHaskell #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Control.Lens
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Lens


data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     } deriving Show

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 } deriving Show

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 } deriving Show

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       } deriving Show

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = _street . _bornAt

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth =  set (born . bornOn . months) 

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (born . bornAt . street) f  . over (address . street) f
