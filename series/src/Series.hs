module Series (slices) where
import Data.List (tails)
import Data.Char (digitToInt)

slices 0 = const [[]]
slices i = windows . map digitToInt
  where windows = takeWhile ((==i) . length) . map (take i) . tails