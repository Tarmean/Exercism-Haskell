module Pangram (isPangram) where
import qualified Data.Set as S
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = S.fromList ['a'..'z'] `S.isSubsetOf` S.fromList (toLower <$> text)
