module PigLatin (translate) where

translate :: String -> String
translate = unwords . map single . words
  where single w@(x:xs) = if x `elem` "aeiou"
                          then w  ++ "ay"
                          else tail ++ head ++ "ay"

