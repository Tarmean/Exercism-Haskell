module School (School, add, empty, grade, sorted) where
import Data.Map as M
import Data.List (sort)

-- The task is to create the data type `School` and
-- implement the functions below.
type School = Map Int [String]

add :: Int -> String -> School -> School
add = (. return) . M.insertWith ((sort .) . (++))

grade :: Int -> School -> [String]
grade = (maybe ([]) id .) . M.lookup

sorted = toList
