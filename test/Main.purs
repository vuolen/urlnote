module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec (describe, it)
import Data.BigInt (fromInt, pow)

import URLNote.Main (encode, decode, toBase83)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "program" do
    it "encodes and decodes \"hello\" to itself" do
      (decode <<< encode) "hello" `shouldEqual` "hello"
  describe "toBase83" do
    it "encodes 0 as an empty string" do
      toBase83 (fromInt 0) `shouldEqual` ""
    it "encodes 1 as b" do
      toBase83 (fromInt 1) `shouldEqual` "b"
    it "encodes 82 as =" do
      toBase83 (fromInt 82) `shouldEqual` "="
    it "encodes 83 as ba" do
      toBase83 (fromInt 83) `shouldEqual` "ba"
    it "encodes 84 as bb" do
      toBase83 (fromInt 84) `shouldEqual` "bb"
    it "encodes 165 as b=" do
      toBase83 (fromInt 165) `shouldEqual` "b="
    it "encodes 166 as ca" do
      toBase83 (fromInt 166) `shouldEqual` "ca"
    it "encodes 7058 as ca" do
      toBase83 (fromInt 7058) `shouldEqual` "bcd"
    it "doesnt encode a large number as 0" do
      toBase83 (pow (fromInt 2147483647) (fromInt 100)) `shouldNotEqual` "a"

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