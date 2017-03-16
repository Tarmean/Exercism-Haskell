module DNA (toRNA) where

-- | if string contains invalid character, return Nothing
-- | if string contains only valid nucleotides, return Just transcription
toRNA :: String -> Maybe String
toRNA = traverse  $  flip lookup [ ('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]
