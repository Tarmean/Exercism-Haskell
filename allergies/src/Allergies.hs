module Allergies (Allergen(..), allergies, isAllergicTo) where
import Data.Bits

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats deriving (Enum, Show, Eq)

allergies i = [allergen | allergen <- [Eggs ..], allergen `isAllergicTo` i]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen i =  representation .&. i /= 0
  where representation = shiftL 1 $ fromEnum allergen
