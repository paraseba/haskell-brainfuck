module Tape (
  Tape(Tape)
  ,rTape, wTape
  ,inc, dec
  ,right, left
  ,blankTape
) where

import Data.Int (Int8)

data Tape = Tape [Int8] Int8 [Int8] deriving (Show)

wTape :: Int8 -> Tape -> Tape
wTape b (Tape l _ r) = Tape l b r

rTape :: Tape -> Int8
rTape (Tape _ current _) = current

update :: (Int8->Int8) -> Tape -> Tape
update f t = wTape (f $ rTape t) t

inc :: Tape -> Tape
inc = update (+1)

dec :: Tape -> Tape
dec = update (+(-1))

-- Move the head right
right :: Tape -> Tape
right (Tape l c (r:rs)) = Tape (c:l) r rs

-- Move the head left
left :: Tape -> Tape
left (Tape (l:ls) c r) = Tape ls l (c:r)

constTape :: Int8 -> Tape
constTape b = Tape [] b (repeat b)

blankTape :: Tape
blankTape = constTape 0
