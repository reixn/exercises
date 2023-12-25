{-# LANGUAGE MultiWayIf #-}

module Rot13 (rot13) where

import Data.Char

cipher :: Int -> Char -> Char
cipher b c =
  let s = 13 + (ord c - b)
   in chr (b + if s >= 26 then s - 26 else s)

rot13 :: String -> String
rot13 =
  fmap
    ( \c ->
        if
          | isAsciiLower c ->
              cipher (ord 'a') c
          | isAsciiUpper c -> cipher (ord 'A') c
          | otherwise -> c
    )