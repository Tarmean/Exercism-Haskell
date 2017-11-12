module Diamond (diamond) where

diamond :: Char -> [String]
diamond c = rows ++ reverse (init rows)
  where
    rows = map mkLine [0..len]
    len = fromEnum c - fromEnum 'A'
    pad s = spaces ++ s ++ spaces
      where
         spaces = replicate (len - length s`div` 2) ' '
    mkLine i
      | i == 0 = pad "A"
      | otherwise = pad $ [cur] ++ innerPadding ++ [cur]
      where
        cur = toEnum (i + fromEnum 'A')
        innerPadding = replicate (pred i * 2 + 1) ' '
