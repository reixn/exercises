module Codewars.Kata.Deletion (deleteNth) where

import Data.Foldable
import Data.IntMap.Strict qualified as IM

deleteF :: IM.IntMap Int -> Int -> [Int] -> [Int]
deleteF _ _ [] = []
deleteF count n (x : xs) =
  let tl = deleteF (IM.insertWith (+) x 1 count) n xs
   in case IM.lookup x count of
        Just v -> if v < n then x : tl else tl
        Nothing -> x : tl

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = deleteF IM.empty n lst