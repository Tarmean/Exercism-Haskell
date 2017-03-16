module Frequency (frequency) where

import Data.Char (toLower, isAlpha)
import Control.Parallel.Strategies (using, parBuffer, rseq)
import qualified Data.Map as M
import qualified Data.Text as T

frequency n t = merge frequencies
  where
    merge = foldl join M.empty
    join = M.unionWith (+)
    -- still don't understand what whnf is
    frequencies = countLines `using` parBuffer n rseq
    countLines = map countLetters t

countLetters = count . clean
  where
    clean = T.filter isAlpha . T.map toLower
    count = T.foldl incLetter M.empty
    incLetter m c = M.insertWith (+) c 1 m
