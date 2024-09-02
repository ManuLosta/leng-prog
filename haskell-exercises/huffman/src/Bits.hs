module Bits (Bit, Bits) where

data Bit = F | T deriving (Eq, Show)

type Bits = [Bit]
