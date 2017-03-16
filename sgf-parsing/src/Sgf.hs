module Sgf (parseSgf) where

import Text.Parsec
import Text.Parsec.Char
import Data.Map  (Map)
import Data.Text (Text)
import Data.Tree (Tree)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf = undefined

entry = mapping <|> block
mapping =  char ';' >> many1 letter  >> many1 translation
translaton =  char '[' >> many1 letter >> char ']'
block :: Parsec Text u Char
block = char '(' >> many1 entry >> char ')'
