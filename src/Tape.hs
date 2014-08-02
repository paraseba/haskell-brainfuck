module Tape (
  Tape(Tape)
  ,rTape, wTape
  ,inc, dec
  ,right, left
  ,blankTape
) where

import Data.Int (Int8)

-- Tape left-tape current right-tape
data Tape t = Tape [t] t [t] deriving (Show)

wTape :: t -> Tape t -> Tape t
wTape b (Tape l _ r) = Tape l b r

rTape :: Tape t -> t
rTape (Tape _ current _) = current

update :: (t -> t) -> Tape t -> Tape t
update f t = wTape (f $ rTape t) t

inc :: Num a => Tape a -> Tape a
inc = update (+1)

dec :: Num a => Tape a -> Tape a
dec = update (+(-1))

-- Move the head right
right :: Tape a -> Tape a
right (Tape l c (r:rs)) = Tape (c:l) r rs

-- Move the head left
left :: Tape a -> Tape a
left (Tape (l:ls) c r) = Tape ls l (c:r)

constTape :: t -> Tape t
constTape b = Tape [] b (repeat b)

blankTape :: Tape Int8
blankTape = constTape 0
