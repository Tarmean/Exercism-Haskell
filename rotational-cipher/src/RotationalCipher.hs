module RotationalCipher (rotate) where
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

rotate :: Int -> String -> String
rotate = map . trans
  where
    trans = (fromMaybe <*>) . flip lookup . table

    table = liftM2 (++) (flip mkTable ['a'..'z']) (flip mkTable ['A'..'Z'])
    mkTable = (zip <*>) . rot
    rot = (uncurry (flip (++)) .) . splitAt
