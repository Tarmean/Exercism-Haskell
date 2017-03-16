module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean (x, y, z) = testPythagorean x y z

testPythagorean x y z = x' + y' == z'
                        ||x' + z' == y'
                        ||y' + z' == x'
  where (x', y', z') = (x*x, y*y, z*z)

mkTriplet x y z = (x, y, z) 
pythagoreanTriplets low high =  [(x, y, z) | x <- [low .. high],
                                             y <- [x .. high],
                                             z <- [y .. high],
                                             testPythagorean x y z]