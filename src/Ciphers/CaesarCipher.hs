module Ciphers.CaesarCipher (encrypt, replace, shiftChar) where

import Data.Char (chr, ord)

replace x y = map (\z -> if z == x then y else z)

shiftChar :: Int -> Char -> Char
shiftChar n = chr . (+ n) . ord

encrypt :: Int -> String -> String
encrypt shift = replace (chr (shift + 32)) ' ' . map (shiftChar shift)

main =
  let originalMessage = "dCode Caesar"
      solution = "gFrgh Fdhvdu"
      encrypted = encrypt 3 originalMessage
   in do
        print ("Original Message: " ++ originalMessage)
        print ("Encrypted Message: " ++ encrypted)
        print (if encrypted == solution then "PASSED" else "FAILED")