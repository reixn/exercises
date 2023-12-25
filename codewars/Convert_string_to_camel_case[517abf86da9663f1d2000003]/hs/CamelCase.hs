module CamelCase (toCamelCase) where

import Data.Char (toUpper)

toCamelCase :: String -> String
toCamelCase "" = ""
toCamelCase [c] = [c]
toCamelCase ('_' : x : xs) = toUpper x : toCamelCase xs
toCamelCase ('-' : x : xs) = toUpper x : toCamelCase xs
toCamelCase (x : xs) = x : toCamelCase xs