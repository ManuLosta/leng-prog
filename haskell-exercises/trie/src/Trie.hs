module Trie (Trie (..), left, right, find, decode, toList) where

import Bit

data Trie a
  = (Trie a) :-: (Trie a)
  | Leaf a
  deriving (Show, Eq)

left :: Trie a -> Trie a
left (l :-: _) = l
left _ = error "Can't get left"

right :: Trie a -> Trie a
right (_ :-: r) = r
right _ = error "Can't get right"

find :: Bits -> Trie a -> a
find [] (Leaf a) = a
find [] _ = error "Didn't get to a Leaf"
find (T : bs) trie = find bs (right trie)
find (F : bs) trie = find bs (left trie)

decode :: Bits -> Trie Char -> String
decode bits trie = decodeChar bits trie
  where
    decodeChar :: Bits -> Trie Char -> String
    decodeChar [] (Leaf c) = [c]
    decodeChar [] _ = []
    decodeChar bs (Leaf c) = c : decode bs trie
    decodeChar (F : bs) trie' = decodeChar bs (left trie')
    decodeChar (T : bs) trie' = decodeChar bs (right trie')

toList :: Trie a -> [(a, Bits)]
toList = toList' []
  where
    toList' :: Bits -> Trie a -> [(a, Bits)]
    toList' bits (Leaf a) = [(a, bits)]
    toList' bits (l :-: r) = toList' (bits ++ [F]) l ++ toList' (bits ++ [T]) r
