module Frequencies (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

type Frequency = (Int, Char)

frequencies :: String -> [Frequency]
frequencies str = insertionSort (map swap (Map.toList (frequencyMap str)))

frequencyMap :: (Ord a) => [a] -> Map a Int
frequencyMap = foldr insert' Map.empty
  where
    insert' x = Map.insertWith (+) x 1

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x < y = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x : xs) = insert x (insertionSort xs)
