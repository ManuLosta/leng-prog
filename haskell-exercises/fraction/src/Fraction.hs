module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Helper methods

num :: Fraction -> Int
num (a, _) = a

den :: Fraction -> Int
den (_, a) = a

simplify :: Fraction -> Fraction
simplify (a, b) = (div a c, div b c)
  where
    c = hcf a b

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add n d = simplify (numerator, denominator)
  where
    denominator = den n * den d
    numerator = num n * den d + num d * den n

-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub n d = simplify (numerator, denominator)
  where
    denominator = den n * den d
    numerator = num n * den d - num d * den n

-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul n d = simplify (numerator, denominator)
  where
    numerator = num n * num d
    denominator = den n * den d

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide n d = simplify (numerator, denominator)
  where
    numerator = num n * den d
    denominator = den n * num d

-- Implement the `hcf` Function

hcf :: Int -> Int -> Int
hcf n 0 = n
hcf n d = hcf d (mod n d)
