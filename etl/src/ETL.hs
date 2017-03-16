module ETL (transform) where

import Data.Map (toList, fromList)
import Data.Char (toLower)

transform m = fromList $ uncurry (map . flip ((,) . toLower)) =<<  toList m