module Huffman (huffmanTrie, encode, decode) where

import Bits (Bits)
import qualified Data.Map as M
import Frequencies (Frequency, frequencies)
import Trie

huffmanTrie :: String -> Trie Char
huffmanTrie input = buildTrie (frequencies input)
  where
    buildTrie :: [Frequency] -> Trie Char
    buildTrie (n : m : ts) = ()

encode :: String -> Trie Char -> Bits
encode input code = error "Implement it"

decode :: Bits -> Trie Char -> String
decode bits trie = error "Implement it"
