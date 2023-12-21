module Reverse(reverseWords) where

import qualified Data.List as L

reverseIter :: String -> String -> String
reverseIter acc [] = acc
reverseIter acc (' ' : xs) =
  let (sp, x) = L.span (== ' ') xs in
  acc ++ (' ' : sp ++ reverseIter [] x)
reverseIter acc (x : xs) = reverseIter (x : acc) xs

reverseWords :: String -> String
reverseWords = reverseIter []
