module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, (!))
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultGarden :: String -> Map String [Plant]
defaultGarden s = garden children s
  where 
    children =          ["Alice", "Bob", "Charlie", "David",
                          "Eve", "Fred", "Ginny", "Harriet",
                          "Ileana", "Joseph", "Kincaid", "Larry"]

garden :: [String] -> String -> Map String [Plant]
garden c plantlist = fromList . zipWith ($) (fmap (,)  children) . map (map (fromJust . search)) $ zipWith (++) l r
  where 
    [l, r] = map (chunksOf 2) $ lines plantlist
    search = flip lookup $ [('V', Violets),
                            ('R', Radishes),
                            ('G', Grass),
                            ('C', Clover)]
    children = sort c




lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants s m = m ! s 
