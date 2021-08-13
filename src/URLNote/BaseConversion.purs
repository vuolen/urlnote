module URLNote.BaseConversion where

import Prelude (class Eq, map, (<<<))
import Data.Array ((!!), range, zip, length)
import Data.HashMap (HashMap, fromArray, lookup)
import Data.Maybe (Maybe)
import Data.Hashable (class Hashable, hash)
import Data.String.CodePoints (CodePoint, codePointFromChar, singleton, toCodePointArray)

-- DIGIT

newtype Digit = Digit CodePoint

derive instance eqDigit :: Eq Digit

instance hashableDigit :: Hashable Digit where
      hash (Digit cp) = (hash <<< singleton) cp

-- ALPHABET

data Alphabet = Alphabet Int (HashMap Int Digit) (HashMap Digit Int)

fromCodePointArray :: Array CodePoint -> Alphabet
fromCodePointArray arr = Alphabet n indexToDigitMap digitToIndexMap
      where n = length arr
            digits = map Digit arr
            indices = range 0 n
            indexToDigitMap = fromArray (zip indices digits)
            digitToIndexMap = fromArray (zip digits indices)

fromString :: String -> Alphabet
fromString = fromCodePointArray <<< toCodePointArray

indexToDigit :: Alphabet -> Int -> Maybe Digit
indexToDigit (Alphabet _ indexToDigitMap _) index = lookup index indexToDigitMap

digitToIndex :: Alphabet -> Digit -> Maybe Int
digitToIndex (Alphabet _ _ digitToIndexMap) index = lookup index digitToIndexMap