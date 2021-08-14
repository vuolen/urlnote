module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec (describe, it)
import Data.BigInt (fromInt, pow)

import URLNote.Main (encode, decode, toBase83, fromBase83, toBase10FFFF, fromBase10FFFF)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "program" do
    it "encodes and decodes \"hello\" to itself" do
      (decode <<< encode) "hello" `shouldEqual` "hello"
  describe "base 83 conversion" do
    it "encodes and decodes \"hello\" to itself" do
      (toBase83 <<< fromBase83) "hello" `shouldEqual` "hello"
  describe "base 10FFFF conversion" do
    it "encodes and decodes \"hello\" to itself" do
      (toBase10FFFF <<< fromBase10FFFF) "hello" `shouldEqual` "hello"

{- import Effect (Effect)
import Data.String.Utils (toCharArray)
import Test.QuickCheck (quickCheck, (===), Result(Failed))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Data.String.CodePoints (singleton)
import Data.Enum (toEnum)
import Data.Maybe
import URLNote.Main (encode) -}

{- base83_digits :: Array String
base83_digits = toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._:/?#[]@!$&'()*+,;="

data Base83Digit = Base83Digit Int

instance semiringBase83Digit :: Semiring Base83Digit where
  add (Base83Digit x) (Base83Digit y) = Base83Digit ((x + y) `mod` 83)
  zero = Base83Digit 0
  mul (Base83Digit x) (Base83Digit y) = Base83Digit $ (x * y) `mod` 83
  one = Base83Digit 1

instance arbBase83Digit :: Arbitrary Base83Digit where
  arbitrary = map (Base83Digit <<< (_ `mod` 83)) arbitrary

toString :: Base83Digit -> Maybe String
toString (Base83Digit x) = map singleton (toEnum x)

runInt :: Base83Digit -> Int
runInt (Base83Digit x) = x

data Base255Digit = Base255Digit Int

instance semiringBase255Digit :: Semiring Base255Digit where
  add (Base255Digit x) (Base255Digit y) = Base255Digit ((x + y) `mod` 255)
  zero = Base255Digit 0
  mul (Base255Digit x) (Base255Digit y) = Base255Digit $ (x * y) `mod` 255
  one = Base255Digit 1

main :: Effect Unit
main = do
  quickCheck singleDigits

singleDigits :: Base83Digit -> Result
singleDigits digit = case toString digit of
                  Just string -> encode string === string
                  Nothing -> Failed $ "Could not convert digit " <> show (runInt digit) <> " to a string"

twoDigits :: Base83Digit -> Base83Digit -> Result
twoDigits first second =  -}