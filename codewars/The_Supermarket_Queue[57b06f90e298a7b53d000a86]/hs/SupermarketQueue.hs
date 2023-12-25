module SupermarketQueue (queueTime) where

import Data.List qualified as L

queueIter :: [Int] -> [Int] -> Int
queueIter ts [] = last ts
queueIter ts (x : xs) =
  queueIter (L.insert (x + head ts) (tail ts)) xs

queueTime :: [Int] -> Int -> Int
queueTime [] _ = 0
queueTime cs tills
  | length cs <= tills = maximum cs
  | otherwise =
      let (initTill, other) = L.splitAt tills cs
       in queueIter (L.sort initTill) other
