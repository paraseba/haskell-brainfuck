{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Test.Parser (properties) where

import Test.QuickCheck
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either
import Control.Monad (liftM3)

import Parser
import Test.Helper

instance Arbitrary Op where
  arbitrary = frequency [(6, elements [IncP,DecP,Inc,Dec,PutByte,GetByte])
                        ,(1, fmap Loop arbitrary)]

prop_ParseEmptyProgram :: Bool
prop_ParseEmptyProgram = either (const False) (==[]) $ parseProgram input
  where input = pack ""

prop_BasicProgram :: Bool
prop_BasicProgram = either (const False) (==expected) $ parseProgram input
  where input = pack "+>>-<."
        expected = [Inc, IncP, IncP, Dec, DecP, PutByte]

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
        expected = [Inc, Dec]

prop_GoodLoop :: Bool
prop_GoodLoop =
  either (const False) (==expected) $ parseProgram input
  where input = pack "\n   + [+><-\n [-+]\n - ]   +  \n"
        expected = [ Inc
                    ,(Loop [ Inc, IncP, DecP, Dec
                            ,(Loop [Dec, Inc])
                            ,Dec
                           ])
                    ,Inc]

properties :: [(String, Prop)]
properties =
  [ ("parse empty", Prop prop_ParseEmptyProgram)
   ,("parse simple ops", Prop prop_BasicProgram)
   ,("parse arbitrary program", Prop prop_CanParseGenericProgram)
   ,("can't parse bad symbols program", Prop prop_CantParseBadSymbols)
   ,("parses spaces and newlines", Prop prop_ParseBlanks)
   ,("parses loops", Prop prop_GoodLoop)
  ]
