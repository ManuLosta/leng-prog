module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatz2 n 0 

collatz2 :: Integer -> Integer -> Maybe Integer
collatz2 n i 
  | n <= 0 = Nothing
  | n == 1 = Just i
  | even n = collatz2 (n `div` 2) (i + 1)
  | otherwise = collatz2 (3*n + 1) (i + 1)
