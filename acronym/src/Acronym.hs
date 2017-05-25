module Acronym (abbreviate) where
import Data.Char (isAlpha, toUpper, isUpper)
import Data.Function (on)
import Data.Foldable (fold)
import Data.Monoid (Any(..))

abbreviate :: String -> String
------------------------------------------------------------------------
abbreviate=map(toUpper.snd)
 .filter(getAny.fold((Any.).uncurry.((&&).not`on`)<$>[isAlpha,isUpper]))
 .(zip=<<(' ':))
