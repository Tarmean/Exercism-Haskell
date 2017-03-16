module Bob (responseFor) where
import Data.Char
import Data.String.Utils

responseFor = answer . strip
answer 
      -- | (==) =<< map toUpper $ x = "Whoa, chill out!"
      | all isSpace             $ x = "Fine. Be that way!"
      | not (any  isLower x) && any isUpper x = "Whoa, chill out!"
      | (==) '?' . last      $ x = "Sure."
      | otherwise                = "Whatever."
