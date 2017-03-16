module Series (largestProduct) where
import Data.List

largestProduct len source 
  | len == 0             = Just 1
  | len < 0              = Nothing
  | length source < len  = Nothing
  | otherwise            = maximum <$> map product <$> slices
  where
    slices  = windows <$> mapM toNum source
    toNum   = flip elemIndex ['0'..'9']
    windows = takeWhile ((== len) . length) . map (take len) . tails

