{-# LANGUAGE FlexibleInstances #-}

module Test.Tape (properties) where

import Test.QuickCheck
import Control.Monad (liftM3)
import Data.Int (Int8)
import Debug.Trace

import Test.Helper
import Tape

instance Arbitrary (Tape Int8) where
  arbitrary = liftM3 Tape arbitrary arbitrary arbitrary

prop_IncAdds :: Tape Int8 -> Property
prop_IncAdds t = rTape t < maxBound ==> (rTape . inc) t == rTape t + 1

prop_DecSubstracts :: Tape Int8 -> Property
prop_DecSubstracts t = rTape t > minBound ==> (rTape . dec) t == rTape t - 1

applyN :: Int -> (a -> a) -> a -> a
applyN n f t = (iterate f t) !! n

prop_IncDec :: Positive Int -> Tape Int8 -> Bool
prop_IncDec (Positive n) tape = rTape tape == rTape tape'
  where tape' = (applyN n inc . applyN n dec) tape

prop_RightLeft :: Positive Int -> Tape Int8 -> Property
prop_RightLeft (Positive n) tape@(Tape _ _ rightL) =
  n <= length rightL ==>
    rTape tape == rTape tape'
    where tape' = (applyN n left . applyN n right) tape

prop_BlankTape :: Bool
prop_BlankTape = rTape t' == 0
  where t' = applyN 100 right $ blankTape

properties :: [(String, Prop)]
properties =
  [ ("inc adds to current", Prop prop_IncAdds)
   ,("dec substracts from current", Prop prop_DecSubstracts)
   ,("dec n inc n ", Prop prop_IncDec)
   ,("right n left n ", Prop prop_RightLeft)
   ,("blank tape is blank", Prop prop_BlankTape)
  ]
