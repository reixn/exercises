{-# LANGUAGE MultiWayIf #-}

module TopWords (top3) where

import Data.Char
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L

data Words = Words
  { count :: {-# UNPACK #-} !Int,
    word :: !String
  }

data TopWords = TopWords !Words !Words !Words

type WordMap = HM.HashMap String Int

addWord :: String -> WordMap -> WordMap
addWord [] wm = wm
addWord s wm
  | all (== '\'') s = wm
  | otherwise = HM.insertWith (+) s 1 wm

wordCntF :: String -> WordMap -> String -> WordMap
wordCntF s acc [] = addWord s acc
wordCntF s acc (x : xs)
  | isAlpha x || x == '\'' = wordCntF (toLower x : s) acc xs
wordCntF s acc (_ : xs) = wordCntF [] (addWord s acc) xs

wordCount :: String -> WordMap
wordCount s = HM.delete [] (wordCntF [] HM.empty s)

topWords :: WordMap -> TopWords
topWords =
  HM.foldlWithKey'
    ( \w@(TopWords w1@(Words c1 _) w2@(Words c2 _) w3@(Words c3 _)) s c ->
        if
          | c > c1 -> TopWords (Words c s) w1 w2
          | c > c2 -> TopWords w1 (Words c s) w2
          | c > c3 -> TopWords w1 w2 (Words c s)
          | otherwise -> w
    )
    (TopWords (Words 0 []) (Words 0 []) (Words 0 []))

top3 :: [Char] -> [[Char]]
top3 s =
  let TopWords
        (Words _ s1)
        (Words _ s2)
        (Words _ s3) = topWords (wordCount s)
   in reverse <$> filter (not . null) [s1, s2, s3]
