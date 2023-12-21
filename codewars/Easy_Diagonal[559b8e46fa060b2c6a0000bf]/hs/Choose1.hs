module Codewars.Kata.Choose1 (diagonal) where

import Data.List qualified as L

-- | diagonal from last num and tail of last diagonal
diagL :: Integer -> [Integer] -> [Integer]
diagL _ [] = []
diagL _ [_] = []
diagL l (x : xs) =
  let cur = l + x
   in cur : diagL cur xs

iterDiag :: Integer -> [Integer] -> [Integer]
iterDiag 0 d = d
iterDiag n l = iterDiag (n - 1) (1 : diagL 1 (tail l))

diagonal :: Integer -> Integer -> Integer
diagonal n p = sum (iterDiag p (L.replicate (fromIntegral (n + 1)) 1))
