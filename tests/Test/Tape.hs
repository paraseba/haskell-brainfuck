{-# LANGUAGE FlexibleInstances #-}

module Test.Tape (properties) where

import Test.QuickCheck
import Control.Monad (liftM3, (>=>))
import Data.Int (Int8)
import Data.Either (isLeft)
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

applyNM :: Monad m => Int -> (a -> m a) -> a -> m a
applyNM 1 k a = k a
applyNM n k a = k a >>= applyNM (n-1) k

prop_IncDec :: Positive Int -> Tape Int8 -> Bool
prop_IncDec (Positive n) tape = rTape tape == rTape tape'
  where tape' = (applyN n inc . applyN n dec) tape

backNForth :: Int -> Tape a -> Either (ExecutionError a) (Tape a)
backNForth n = applyNM n right >=> applyNM n left

forthNBack :: Int -> Tape a -> Either (ExecutionError a) (Tape a)
forthNBack n = applyNM n left >=> applyNM n right

prop_RightLeftWithinLimits :: Positive Int -> Tape Int8 -> Property
prop_RightLeftWithinLimits (Positive n) tape@(Tape _ _ rightL) =
  n <= length rightL ==>
    either (const False) ((== rTape tape) . rTape) $ backNForth n tape

prop_RightLeftBeyondLimits :: Positive Int -> Tape Int8 -> Property
prop_RightLeftBeyondLimits (Positive n) tape@(Tape _ _ rightL) =
  n > length rightL ==> isLeft $ backNForth n tape

prop_LeftRightWithinLimits :: Positive Int -> Tape Int8 -> Property
prop_LeftRightWithinLimits (Positive n) tape@(Tape leftL _ _) =
  n <= length leftL ==>
    either (const False) ((== rTape tape) . rTape) $ forthNBack n tape

prop_LeftRightBeyondLimits :: Positive Int -> Tape Int8 -> Property
prop_LeftRightBeyondLimits (Positive n) tape@(Tape leftL _ _) =
  n > length leftL ==> isLeft $ forthNBack n tape

prop_BlankTapeCanGoRight :: Bool
prop_BlankTapeCanGoRight =
  either (const False) ((==0) . rTape) $ applyNM 100 right $ blankTape

prop_BlankTapeCantGoLeft :: Positive Int -> Bool
prop_BlankTapeCantGoLeft (Positive n) =
  isLeft $ applyNM n left blankTape

properties :: [(String, Prop)]
properties =
  [ ("inc adds to current", Prop prop_IncAdds)
   ,("dec substracts from current", Prop prop_DecSubstracts)
   ,("dec n inc n ", Prop prop_IncDec)
   ,("right n left n, with n<=length ", Prop prop_RightLeftWithinLimits)
   ,("right n left n, with n>length ", Prop prop_RightLeftBeyondLimits)
   ,("left n right n, with n<=length ", Prop prop_LeftRightWithinLimits)
   ,("left n right n, with n>length ", Prop prop_LeftRightBeyondLimits)
   ,("blank tape is blank to the right", Prop prop_BlankTapeCanGoRight)
   ,("blank tape is at leftmost position", Prop prop_BlankTapeCantGoLeft)
  ]
