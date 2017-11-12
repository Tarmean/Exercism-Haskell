{-# Language RecordWildCards #-}
module ZebraPuzzle (Resident(..), Solution(..), solve) where
import Data.List (zip5, find)
import Data.List as L
import Control.Monad (guard)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Bounded, Enum)
data Drinks = Tea | OrangeJuice | Water | Coffee | Milk
  deriving (Eq, Show, Bounded, Enum)
data Cigaretts = OldGold | Kools | LuckyStrike | Parliaments | Chesterfields
  deriving (Eq, Show, Bounded, Enum)
data House = Red | Green | Yellow | Blue | Ivory
  deriving (Eq, Show, Bounded, Enum)
data Pet = Dog | Snails | Fox | Zebra | Horse
  deriving (Eq, Show, Bounded, Enum)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve = Solution { waterDrinker = waterDrinker, zebraOwner = zebraOwner }
  where
    solution = head solutions

    drinksWater Position {..} = drink == Water
    ownsZebra   Position {..}   = pet == Zebra
    getResident (Just Position {..}) = resident

    zebraOwner   = getResident $ find ownsZebra solution
    waterDrinker = getResident $ find drinksWater solution
data Position = Position { resident :: Resident, drink :: Drinks, cigarette :: Cigaretts, house :: House, pet :: Pet } deriving Show

solutions :: [[Position]]
solutions = do 
              -- tried putting the contained conditions first and the rest kind of followed
              -- not sure if optimal, though
              house <- permutations
              rightTo  house Green           house Ivory

              resident <- permutations
              position 1                     resident Norwegian
              same     resident Englishman   house Red
              nextTo   resident Norwegian    house Blue

              drinks <- permutations
              position 3                     drinks Milk
              same     drinks Coffee         house Green
              same     resident Ukrainian    drinks Tea

              smokes <- permutations
              same     smokes Kools          house Yellow
              same     resident Japanese     smokes Parliaments
              same     smokes LuckyStrike    drinks OrangeJuice

              owns <- permutations
              same     smokes OldGold        owns Snails
              nextTo   smokes Chesterfields  owns Fox
              nextTo   smokes Kools          owns Horse

              same     resident Spaniard     owns Dog

              return $ zipWith5 Position resident drinks smokes house owns

           
  where
    position pos kind value = guard (kind !! (pos-1) == value)

    same  k1 v1 k2 v2 = guard (elem (v1, v2) $ zip k1 k2)

    rightToPred k1 v1 k2 v2 = elem (v1, v2) $ zip k1 (tail k2)
    rightTo k1 v1 k2 v2 = guard (rightToPred k1 v1 k2 v2)
    nextTo k1 v1 k2 v2 = guard (rightToPred k1 v1 k2 v2||
                                rightToPred k2 v2 k1 v1)

    permutations :: (Bounded a, Enum a) => [[a]]
    permutations = L.permutations [minBound..maxBound]
           

