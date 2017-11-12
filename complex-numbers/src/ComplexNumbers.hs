module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Complex !a !a  deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex  = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

abs :: Floating a => Complex a -> a
abs (Complex l r) = sqrt $ (l*l) + (r*r)

real :: Num a => Complex a -> a
real (Complex r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex r1 i1) (Complex r2 i2) = Complex ((r1 * r2 + i1 * i2) / divisor) ((i1 * r2 - r1 * i2) / divisor)
  where divisor = r2^2 + i2^2
