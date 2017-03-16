module WordCount (wordCount) where
import qualified Data.Map as M
import Data.List
import Data.Char (toLower, isAlphaNum)
import Data.List.Split (wordsBy)

wordCount = M.fromListWith (+) . map (flip (,) 1) . wordsBy (not . isAlphaNum) . map toLower