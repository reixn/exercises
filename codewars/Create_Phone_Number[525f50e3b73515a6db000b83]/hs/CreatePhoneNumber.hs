module CreatePhoneNumber (createPhoneNumber) where

import Data.Char (intToDigit)

createPhoneNumber :: [Int] -> String
createPhoneNumber is =
  case intToDigit <$> is of
    [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9] -> ['(', i0, i1, i2, ')', ' ', i3, i4, i5, '-', i6, i7, i8, i9]
    _ -> error "invalid phone number"