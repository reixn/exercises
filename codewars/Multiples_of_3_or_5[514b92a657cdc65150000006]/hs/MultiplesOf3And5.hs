module MultiplesOf3And5 (solution) where

sumMultiply :: Integer -> Integer -> Integer
sumMultiply n i
  | i <= n = 0
  | otherwise =
      let cnt = (i - 1) `div` n in n * (cnt + 1) * cnt `div` 2

solution :: Integer -> Integer
solution number = sumMultiply 3 number + sumMultiply 5 number - sumMultiply 15 number