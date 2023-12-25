module Codewars.Kata.Hashtag (generateHashtag) where

import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag s =
  case words s of
    [] -> Nothing
    ws ->
      let hashTag = '#' : concatMap (\w -> toUpper (head w) : tail w) ws
       in if length hashTag > 140 then Nothing else Just hashTag