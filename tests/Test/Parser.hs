{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Test.Parser (properties) where

import Debug.Trace
import Test.QuickCheck
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either

import Parser
import Test.Helper


instance Arbitrary PointerOp where
  arbitrary = elements [IncP, DecP]
instance Arbitrary ByteOp where
  arbitrary = elements [Inc, Dec]
instance Arbitrary SideEffectOp where
  arbitrary = elements [PutByte, GetByte]
instance Arbitrary Loop where
  arbitrary = fmap Loop (arbitrary :: Gen [Op])

instance Arbitrary Op where
  arbitrary = oneof [p,b,s,l]
    where p = fmap P (arbitrary :: Gen PointerOp)
          b = fmap B (arbitrary :: Gen ByteOp)
          s = fmap S (arbitrary :: Gen SideEffectOp)
          l = fmap L (arbitrary :: Gen Loop)

prop_ParseEmptyProgram :: Bool
prop_ParseEmptyProgram = either (const False) (==[]) $ parseProgram input
  where input = pack ""

prop_BasicProgram :: Bool
prop_BasicProgram = either (const False) (==expected) $ parseProgram input
  where input = pack "+>>-<."
        expected = [B Inc, P IncP, P IncP, B Dec, P DecP, S PutByte]

newtype Code = Code {unCode :: String} deriving Show

instance Arbitrary Code where
  arbitrary = fmap Code $ listOf $ elements ['>','<','+','-',',','.']

prop_CanParseGenericProgram :: Code -> Bool
prop_CanParseGenericProgram = isRight . parseProgram . pack . unCode

prop_CantParseBadSymbols :: Code -> Code -> Bool
prop_CantParseBadSymbols c1 c2 =
  (isLeft . parseProgram . pack) $ unCode c1 ++ ['~'] ++ unCode c2

prop_ParseBlanks :: Bool
prop_ParseBlanks =
  (isRight . parseProgram . pack) "\n   +\n   +  \n"

properties :: [(String, Prop)]
properties =
  [ ("parse empty", Prop prop_ParseEmptyProgram)
   ,("parse simple ops", Prop prop_BasicProgram)
   ,("parse arbitrary program", Prop prop_CanParseGenericProgram)
   ,("can't parse bad symbols program", Prop prop_CantParseBadSymbols)
   ,("parses spaces and newlines", Prop prop_ParseBlanks)
  ]
