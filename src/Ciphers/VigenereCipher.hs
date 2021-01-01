module Ciphers.VigenereCipher where

import Data.Char (chr, ord)

shiftChar :: Int -> Char -> Char
shiftChar n = chr . (+ n) . ord

shiftWithDifference :: Char -> Char -> Char
shiftWithDifference x = shiftChar difference
  where
    difference = ord x - 65

repeatKey = concat . repeat

encrypt :: String -> String -> String
encrypt key = zipWith shiftWithDifference infiniteKey
  where
    infiniteKey = repeatKey key

main =
  let originalMessage = "MEET AT DAWN"
      solution = "MPPl L_8DLbf"
      encrypted = encrypt "ALLY" originalMessage
   in do
        print ("Original Message: " ++ originalMessage)
        print ("Encrypted Message: " ++ encrypted)
        print (if encrypted == solution then "PASSED" else "FAILED")