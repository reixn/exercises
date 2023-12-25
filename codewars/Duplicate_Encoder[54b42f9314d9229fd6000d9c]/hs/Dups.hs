module Dups (duplicateEncode) where

import Data.Char
import Data.Foldable
import Data.IntMap.Strict qualified as IM

duplicateEncode :: String -> String
duplicateEncode s =
  let ords = fmap (ord . toLower) s
      chars = IM.filter (> 1) (foldl' (\s c -> IM.insertWith (+) c 1 s) IM.empty ords)
   in fmap
        (\c -> if IM.member c chars then ')' else '(')
        ords
