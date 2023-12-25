module MovingZeros (moveZeros) where

moveF :: Int -> [Int] -> [Int]
moveF cnt [] = replicate cnt 0
moveF cnt (0 : xs) = moveF (cnt + 1) xs
moveF cnt (x : xs) = x : moveF cnt xs

moveZeros :: [Int] -> [Int]
moveZeros = moveF 0