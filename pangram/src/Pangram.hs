module Pangram (isPangram) where
import qualified Data.Set as S
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram = S.isSubsetOf (S.fromList ['a'..'z']) . S.fromList . map toLower
