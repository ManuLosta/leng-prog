module ListsPart2 (Bit (..), bitAt, charToBits, bits, queens) where

import Data.Bits (testBit)
import Data.Char (ord)

data Bit = F | T deriving (Eq, Show, Enum, Read)

type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7 - n) then T else F

charToBits :: Char -> Bits
charToBits c = map (`bitAt` c) [0 .. 7]

bits :: String -> Bits
bits s = foldr (++) [] (map charToBits s)

type Solution = [Int]

queens :: Int -> [Solution]
queens size = solve 1 [[]]
  where
    solve i solutions
      | i > size = solutions
      | otherwise = solve (i + 1) [j : solution | j <- [1 .. size], solution <- solutions, safe j 1 solution]
    safe _ _ [] = True
    safe j offset (c : cs) = and [j /= c, j /= c - offset, j /= c + offset, safe j (offset + 1) cs]
