module OCR (convert) where
import Data.List.Split (chunksOf)
import Data.List (intercalate, elemIndex)
import Data.Char (intToDigit)


convert = intercalate "," . map parseLine . chunksOf 4 . lines
  where
    parseLine = map parse . foldl1 (zipWith (++)) . map (chunksOf 3)
    parse = maybe '?' intToDigit . flip elemIndex [" _ | ||_|   "
                                                  ,"     |  |   "
                                                  ," _  _||_    "
                                                  ," _  _| _|   "
                                                  ,"   |_|  |   "
                                                  ," _ |_  _|   "
                                                  ," _ |_ |_|   "
                                                  ," _   |  |   "
                                                  ," _ |_||_|   "
                                                  ," _ |_| _|   "]