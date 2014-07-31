{-# LANGUAGE ExistentialQuantification #-}

module Test.Helper ( Prop(Prop) ) where

import Test.QuickCheck (Testable, property, exhaustive)

data Prop = forall t. Testable t => Prop t

instance Testable Prop where
  property (Prop t) = property t
  exhaustive (Prop t) = exhaustive t

