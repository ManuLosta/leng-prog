module Frequencies (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map (Map)
import qualified Data.Map as Map
import Trie (Trie (..))

type Frequency = (Int, Trie Char)

frequencies :: String -> [Frequency]
frequencies str = insertionSort (map toLeaf (Map.toList (frequencyMap str)))

toLeaf :: (Char, Int) -> (Int, Trie Char)
toLeaf (char, int) = (int, Leaf char)

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
