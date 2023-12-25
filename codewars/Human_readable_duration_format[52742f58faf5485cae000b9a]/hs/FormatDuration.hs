module FormatDuration (formatDuration) where

import Data.Maybe (catMaybes)

components :: (Integral i) => i -> (i, i, i, i, i)
components n =
  let (r1, s) = n `divMod` 60
      (r2, m) = r1 `divMod` 60
      (r3, h) = r2 `divMod` 24
      (y, d) = r3 `divMod` 365
   in (y, d, h, m, s)

showComp :: (Integral i) => i -> String -> Maybe ShowS
showComp 0 _ = Nothing
showComp n suf =
  Just
    ( let ssuf = if n > 1 then (suf ++) . ('s' :) else (suf ++)
       in shows (toInteger n) . (' ' :) . ssuf
    )

concatComp :: [ShowS] -> ShowS
concatComp [] = id
concatComp [x] = x
concatComp [x1, x2] = x1 . (" and " ++) . x2
concatComp (x : xs) = x . (", " ++) . concatComp xs

formatDuration :: (Integral i) => i -> String
formatDuration 0 = "now"
formatDuration n =
  let (y, d, h, m, s) = components n
   in concatComp
        ( catMaybes
            [ showComp y "year",
              showComp d "day",
              showComp h "hour",
              showComp m "minute",
              showComp s "second"
            ]
        )
        ""