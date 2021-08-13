module URLNote.Main where

import Prelude (otherwise, (<<<), (==), (+), (*), map)
import URLNote.BaseConversion (Alphabet(..), Digit(..), fromString)
import URLNote.BaseConversion as BaseConversion
import Data.String.CodePoints (codePointFromChar, fromCodePointArray, toCodePointArray)
import Data.Char (fromCharCode)
import Data.Array (cons, range)
import Data.Array as Array
import Data.HashMap (lookup)
import Data.BigInt (BigInt, fromInt, toNumber, rem, quot, pow)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

unsnoc :: String -> Maybe {init :: String, last :: Digit}
unsnoc "" = Nothing
unsnoc string = case (Array.unsnoc <<< toCodePointArray) string of
      Just {init, last} -> Just {init: (fromCodePointArray init), last: Digit last}
      Nothing -> Nothing

base83_alphabet = fromString "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345689-._:/?#[]@!$&'()*+,;=" :: Alphabet
base256_alphabet = BaseConversion.fromCodePointArray (map (unsafePartial (codePointFromChar <<< fromJust <<< fromCharCode)) (range 0 255)) :: Alphabet
base10FFFF_alphabet = BaseConversion.fromCodePointArray (map (unsafePartial (codePointFromChar <<< fromJust <<< fromCharCode)) (range 0 0x10FFFF)) :: Alphabet


-- BigInt constants
bi0 = fromInt 0 :: BigInt
bi1 = fromInt 1 :: BigInt
bi83 = fromInt 83 :: BigInt
bi256 = fromInt 256 :: BigInt

encode :: String -> String
encode string = (toBase83 <<< fromBase10FFFF) string

baseEncoder :: Alphabet -> (BigInt -> String)
baseEncoder (Alphabet base indexToDigitMap _) = encode
      where encode :: BigInt -> String
            encode num = fromCodePointArray (map (\(Digit x) -> x) (encode' num []))
            encode' :: BigInt -> Array Digit -> Array Digit
            encode' num digits 
                  | num == bi0 = digits
                  | otherwise = 
                        let 
                              nextDigit = unsafePartial (fromJust (lookup (floor (toNumber (rem num biBase))) indexToDigitMap))
                        in 
                              encode' (quot num biBase) (cons nextDigit digits)
            biBase = (fromInt base)

baseDecoder :: Alphabet -> (String -> BigInt)
baseDecoder (Alphabet base _ digitToIndexMap)  = decode
      where decode :: String -> BigInt
            decode string = decode' string bi0 bi0
            decode' :: String -> BigInt -> BigInt -> BigInt
            decode' string index sum = case unsnoc string of
                                                Just {init, last} -> let
                                                      nextIndex = unsafePartial (fromJust (lookup last digitToIndexMap))
                                                in
                                                      decode' init (index + bi1) (sum + (fromInt nextIndex) * (pow biBase index))
                                                Nothing -> sum
            biBase = (fromInt base)

decode :: String -> String
decode string = (toBase10FFFF <<< fromBase83) string

toBase83 :: BigInt -> String
toBase83 = baseEncoder base83_alphabet

toBase10FFFF :: BigInt -> String
toBase10FFFF = baseEncoder base10FFFF_alphabet

fromBase83 :: String -> BigInt
fromBase83 = baseDecoder base83_alphabet

fromBase10FFFF :: String -> BigInt
fromBase10FFFF = baseDecoder base10FFFF_alphabet