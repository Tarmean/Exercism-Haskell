module Triangle (TriangleType(..), triangleType) where
import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType l1 l2 l3
    | not $ is_ok tri = Illegal
    | equals tri == 3 = Equilateral
    | equals tri == 2 = Isosceles
    | equals tri == 1 = Scalene
    where tri = [l1, l2, l3]

equals ls = maximum $ map (length . flip filter ls . (==)) ls

is_ok ls = l1 + l2 >= l3 && all (>0) ls
  where [l1, l2, l3] = sort ls
