module Atbash (decode, encode) where

import Data.Char
import Data.List.Split (chunksOf)
import Control.Applicative

translate = map (single . toLower)
  where single c
            | isAlpha c = toEnum $ fromEnum 'a'
                                 + fromEnum 'z'
                                 - fromEnum  c
            | otherwise = c
clean = filter $ liftA2 (&&) isAscii isAlphaNum

decode = translate . clean
encode = unwords . chunksOf 5 . translate . clean