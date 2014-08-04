{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Tape
Description : Implement the brainfuck tape
Copyright   : (c) Sebastian Galkin, 2014
License     : MIT
Maintainer  : paraseba@gmail.com
Stability   : experimental

Provides a type and operations to implement the brainfuck tape. The tape has
the concept of a pointer, and the pointer can be incremented or decremented.

-}

module Tape (
  Tape(Tape)
  ,ExecutionError(errMsg, errTape)
  ,rTape, wTape
  ,inc, dec
  ,right, left
  ,BFExError
  ,BFTape
  ,blankTape
) where

import Data.Int (Int8)
import Control.Monad.Error (Error, strMsg)

-- | Brainfuck tape. Constructor arguments correspond to
--
--   1. left of the current pointer
--   2. current pointed value
--   3. right of the current pointer
--
-- The left part of the tape is reversed, so the first element of the list
-- is the rightmost position. The right list is normal order, its first element
-- is the leftmost one.
data Tape t = Tape [t] t [t]
  deriving (Show)

-- | Write element to the current position in the tape
wTape :: t      -- ^ The element to write
      -> Tape t -- ^ The tape
      -> Tape t -- ^ The modified tape
wTape b (Tape l _ r) = Tape l b r

-- | Read the pointed element
rTape :: Tape t -- ^ The tape
      -> t      -- ^ The element currently pointed by the pointer
rTape (Tape _ current _) = current

update :: (t -> t) -> Tape t -> Tape t
update f t = wTape (f $ rTape t) t

-- | Increment the currently pointed element
inc :: Num a
    => Tape a -- ^ The tape
    -> Tape a -- ^ The tape with the current position incremented
inc = update (+1)

-- | Decrement the currently pointed element
dec :: Num a
    => Tape a -- ^ The tape
    -> Tape a -- ^ The tape with the current position decremented
dec = update (+(-1))

-- | Type for execution errors, trying to move the tape beyond one of its
-- ends. The 'String' argument is the error message and the 'Tape' is in
-- the state right before the faulting operation
data ExecutionError a = ExecutionError {errMsg :: String,  errTape :: (Tape a)}

-- | Move the pinter to the right
right :: Tape a  -- ^ The tape
      -> Either (ExecutionError a) (Tape a)
      -- ^ A new tape with its pointer pointing to the
      -- element to the right of the pointer in the
      -- original tape; or an execution error if the
      -- tape to the right is exhausted
right t@(Tape _ _ []) = Left $ ExecutionError "Error trying to go right on an empty tape" t
right (Tape l c (r:rs)) = Right $ Tape (c:l) r rs

-- | Move the pinter to the left
left :: Tape a  -- ^ The tape
     -> Either (ExecutionError a) (Tape a)
     -- ^ A new tape with its pointer pointing to
     -- the element to the left of the pointer in
     -- the original tape; or an execution error if
     -- the tape to the left is exhausted
left t@(Tape [] _ _) = Left $ ExecutionError "Error trying to go left on an empty tape" t
left (Tape (l:ls) c r) = Right $ Tape ls l (c:r)

constTape :: t -> Tape t
constTape b = Tape [] b (repeat b)

-- | Execution error type for basic Brainfuck tapes
type BFExError = ExecutionError Int8

-- | Brainfuck tapes type
type BFTape = Tape Int8

instance Error BFExError where
  strMsg s = ExecutionError s (Tape [] 0 [])

-- | A @(0 :: 'Int8')@ initialized, infinite 'Tape' pointing to its leftmost position.
-- An attemp to move the pointer left will result in an error
blankTape :: BFTape
blankTape = constTape 0
