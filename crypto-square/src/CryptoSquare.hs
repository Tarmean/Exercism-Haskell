module CryptoSquare
    ( ciphertext
    , normalizeCiphertext
    , normalizePlaintext
    , plaintextSegments
    ) where
import Data.Char
import Data.List
import Data.List.Split

cipher = transpose . plaintextSegments
ciphertext = concat . cipher
normalizeCiphertext = intercalate " " . cipher

normalizePlaintext = map toLower . filter isAlphaNum

plaintextSegments s = chunksOf cols cleaned
  where
    cleaned = normalizePlaintext s
    cols = ceiling (sqrt $ fromIntegral (length cleaned) )
