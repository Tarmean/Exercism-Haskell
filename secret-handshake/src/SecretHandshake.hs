{-#LANGUAGE FlexibleInstances#-}
module SecretHandshake (handshake) where
import Data.Bits (testBitDefault)
import Control.Applicative (liftA2)
class Shakeable a where
  handshake :: a -> [String]

instance Shakeable Int where
    handshake i = baseshake $ map (testBitDefault i) ([0..4] :: [Int])
instance Shakeable String where
    handshake s
      | allValid s = baseshake . reverse $ map (== '1') s
      | otherwise = []
      where allValid = all $ liftA2 (||) (=='0') (=='1')
 
baseshake b = finalize . map fst . filter snd $ zip entries b
  where entries = ["wink", "double blink", "close your eyes", "jump"]
        finalize = if length b == 5 && last b
                   then reverse
                   else id