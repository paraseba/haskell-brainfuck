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
  ,rTape, wTape
  ,inc, dec
  ,right, left
  ,blankTape
) where

import Data.Int (Int8)

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

-- | Move the pinter to the right
right :: Tape a -- ^ The tape
      -> Tape a -- ^ The pointer in the resulting tape points to the element
                -- to the right of the pointer in the original tape
right (Tape l c (r:rs)) = Tape (c:l) r rs

-- | Move the pinter to the left
left :: Tape a -- ^ The tape
     -> Tape a -- ^ The pointer in the resulting tape points to the element
               -- to the left of the pointer in the original tape
left (Tape (l:ls) c r) = Tape ls l (c:r)

constTape :: t -> Tape t
constTape b = Tape [] b (repeat b)

-- | A @(0 :: 'Int8')@ initialized, infinite 'Tape' pointing to its leftmost position.
-- An attemp to move the pointer left will result in an error
blankTape :: Tape Int8
blankTape = constTape 0
