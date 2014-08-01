{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Test.Parser (properties) where

import Debug.Trace
import Test.QuickCheck
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either
import Control.Monad (liftM3)

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

sizedCode :: Int -> Gen Code
sizedCode 0 = fmap Code $ listOf $ frequency [ (20, elements "+-")
                                             , (10, elements "><")
                                             , (2,  elements ",.")
                                             , (10, elements " \n\t")
                                             ]
sizedCode n | n>0 = liftM3 (\a b c -> Code $ unCode a ++ unCode b ++ unCode c)
                           (sizedCode n'')
                           (fmap (Code . (\s->"[" ++ s ++ "]") . unCode) $ sizedCode n')
                           (sizedCode n'')
  where n'  = n  `div` 2
        n'' = n' `div` 2


instance Arbitrary Code where
  arbitrary = sized sizedCode

prop_CanParseGenericProgram :: Code -> Bool
prop_CanParseGenericProgram = isRight . parseProgram . pack . unCode

prop_CantParseBadSymbols :: Code -> Code -> Bool
prop_CantParseBadSymbols c1 c2 =
  (isLeft . parseProgram . pack) $ unCode c1 ++ ['~'] ++ unCode c2

prop_ParseBlanks :: Bool
prop_ParseBlanks =
  either (const False) (==expected) $ parseProgram input
  where input = pack "\n   +\n   -  \n"
        expected = [B Inc, B Dec]

prop_GoodLoop :: Bool
prop_GoodLoop =
  either (const False) (==expected) $ parseProgram input
  where input = pack "\n   + [+><-\n [-+]\n - ]   +  \n"
        expected = [ B Inc
                    ,L (Loop [ B Inc, P IncP, P DecP, B Dec
                              ,L (Loop [B Dec, B Inc])
                              ,B Dec
                             ])
                    ,B Inc]


properties :: [(String, Prop)]
properties =
  [ ("parse empty", Prop prop_ParseEmptyProgram)
   ,("parse simple ops", Prop prop_BasicProgram)
   ,("parse arbitrary program", Prop prop_CanParseGenericProgram)
   ,("can't parse bad symbols program", Prop prop_CantParseBadSymbols)
   ,("parses spaces and newlines", Prop prop_ParseBlanks)
   ,("parses loops", Prop prop_GoodLoop)
  ]
