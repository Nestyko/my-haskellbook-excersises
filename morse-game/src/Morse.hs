module Morse where

import qualified Data.Map as M

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse =
  M.fromList
    [ ('a', ".-"),
      ('b', "-..."),
      ('c', "-.-."),
      ('d', "-.."),
      ('e', "."),
      ('f', "..-."),
      ('g', "--."),
      ('h', "...."),
      ('i', ".."),
      ('j', ".---"),
      ('k', "-.-"),
      ('l', ".-.."),
      ('m', "--"),
      ('n', "-."),
      ('o', "---"),
      ('p', ".--."),
      ('q', "--.-"),
      ('r', ".-."),
      ('s', "..."),
      ('t', "-"),
      ('u', "..-"),
      ('v', "...-"),
      ('w', ".--"),
      ('x', "-..-"),
      ('y', "-.--"),
      ('z', "--.."),
      ('1', ".----"),
      ('2', "..---"),
      ('3', "...--"),
      ('4', "....-"),
      ('5', "....."),
      ('6', "-...."),
      ('7', "--..."),
      ('8', "---.."),
      ('9', "----."),
      ('0', "-----")
    ]

flipMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
flipMap = M.foldrWithKey (flip M.insert) M.empty

morseToLetter :: M.Map Morse Char
morseToLetter = flipMap letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
  sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseToLetter