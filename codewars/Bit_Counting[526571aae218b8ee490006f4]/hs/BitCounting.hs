module BitCounting (countBits) where

import Data.Bits ((.&.), shiftR)

countIter :: Int -> Int -> Int
countIter acc 0 = acc
countIter acc x = countIter (acc + (x .&. 1)) (x `shiftR` 1) 

countBits :: Int -> Int
countBits = countIter 0
