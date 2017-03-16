module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where
import System.Random(newStdGen, randomRs)
import Data.Char (chr, ord)

caesarProcess :: (Int -> Int -> Int) -> String -> String -> String
caesarProcess f a = map chr . shift . map ord
  where
    shift = map wrap . zipWith op cypher
    op = flip f

    cypher = cycle $ map (subBase . ord) a

    wrap = addBase . (`mod` 26) . (+26) . subBase
    subBase = flip (-) $ ord 'a'
    addBase = flip (+) $ ord 'a'

caesarDecode :: String -> String -> String
caesarDecode = caesarProcess (-)

caesarEncode :: String -> String -> String
caesarEncode = caesarProcess (+)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom s = do
                        cypher <- randomRs ('a', 'z') <$> newStdGen
                        let encoded = caesarEncode cypher s
                        return (cypher, encoded)