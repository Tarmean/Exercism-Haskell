{-# Language TupleSections #-}
module Isogram (isIsogram) where
import Data.Char
import Data.Foldable (foldMap)
import Data.Monoid (All(..))
import qualified Data.Map as M

isIsogram :: String -> Bool
isIsogram = getAll . foldMap (All . (==1)) . M.fromListWith (+) . map (,1) . map toLower . filter isLetter
