module Lists
  ( member,
    union,
    intersection,
    difference,
    insert,
    insertionSort,
    binaryToDecimal,
    toDecimal,
    toDec,
    decimal,
    firsts,
    binaryAdd,
  )
where

import Data.Char (digitToInt)

member :: Int -> [Int] -> Bool
member _ [] = False
member e (x : xs) = e == x || member e xs

union :: [Int] -> [Int] -> [Int]
union [] ys = ys
union (x : xs) ys
  | member x ys = union xs ys
  | otherwise = x : union xs ys

-- Remove Implementations, from, here on

intersection :: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection (x : xs) ys
  | member x ys = x : intersection xs ys
  | otherwise = intersection xs ys

difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference (x : xs) ys
  | member x ys = difference xs ys
  | otherwise = x : difference xs ys

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys)
  | x < y = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x : xs) = insert x (insertionSort xs)

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x : xs) = x * 2 ^ length xs + binaryToDecimal xs

toDecimal :: Int -> [Int] -> Int
toDecimal _ [] = 0
toDecimal base (x : xs) = x * base ^ length xs + binaryToDecimal xs

toDec :: Int -> String -> Int
toDec base s = toDecimal base (map digitToInt s)

-- Same as `toDec` But use a list comprehension

decimal :: Int -> String -> Int
decimal base s = sum [digitToInt a * i | (a, i) <- zip (reverse s) powers]
  where
    powers = [base ^ i | i <- [0 ..]]

firsts :: [a] -> [[a]]
firsts [] = []
firsts (x : xs) = [x] : map (x :) (firsts xs)

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

toBinary :: Int -> String
toBinary 0 = ""
toBinary n
  | n `mod` 2 == 1 = toBinary (n `div` 2) ++ "1"
  | otherwise = toBinary (n `div` 2) ++ "0"

binaryAdd :: String -> String -> String
binaryAdd "" "" = "0"
binaryAdd a b = toBinary (toDec 2 a + toDec 2 b)
