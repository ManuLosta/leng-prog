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
binaryToDecimal xs = sum [a * i | (a, i) <- zip (reverse xs) powers]
  where
    powers = [2 ^ i | i <- [0 ..]]

toDecimal :: Int -> [Int] -> Int
toDecimal base xs = sum [a * i | (a, i) <- zip (reverse xs) powers]
  where
    powers = [base ^ i | i <- [0 ..]]

toDec :: Int -> String -> Int
toDec base s = error "Implement it"

-- Same as `toDec` But use a list comprehension

decimal :: Int -> String -> Int
decimal = error "Implement it"

firsts :: [a] -> [[a]]
firsts = error "Implement it"

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd :: String -> String -> String
binaryAdd = error "Implement it"
