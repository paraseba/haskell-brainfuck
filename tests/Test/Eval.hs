module Test.Eval (properties) where

import Test.QuickCheck
import Debug.Trace
import Control.Monad.State
import Data.Int (Int8)

import Test.Helper
import Parser
import Tape
import Eval

{-ioMachine :: Machine IO-}
{-ioMachine = Machine blankTape (putChar . toEnum) (fromEnum . getChar)-}

evaluate :: Program -> Tape
evaluate p = tape $ evalState (eval simulator p) (emptyState 0)

prop_EmptyProgram :: Bool
prop_EmptyProgram = current == 0
  where (Tape _ current _) = evaluate []

prop_IncrementsDecrements :: Positive Int8 -> Positive Int8 -> Bool
prop_IncrementsDecrements (Positive incs) (Positive decs) =
  current == incs - decs
  where program = replicate (fromIntegral incs) (B Inc) ++
                  replicate (fromIntegral decs) (B Dec)
        (Tape _ current _) = evaluate program

prop_simpleProgram :: Bool
prop_simpleProgram =
  current == 1 && next !! 0 == 4
  where program = [(B Inc), (B Inc),                             -- +2
                   (P IncP), (B Inc), (B Inc), (B Inc), (B Inc), -- right +4
                   (P DecP), (B Dec)                             -- left -1
                  ]
        res = evaluate program
        (Tape prev current next) = res

prop_PutByte :: Bool
prop_PutByte =
  out == [0, 1, 2, 1]
  where program = [(S PutByte), (B Inc), (S PutByte), (B Inc), (S PutByte), (B Dec), (S PutByte)]
        res = execState (eval simulator program) (emptyState 0)
        out = simStateOutput res

prop_GetByte :: Bool
prop_GetByte =
  out == [0, 42]
  where program = [(S PutByte), (B Inc), (S GetByte), (S PutByte)]
        res = execState (eval simulator program) (emptyState 42)
        out = simStateOutput res

prop_DecLoop :: Bool
prop_DecLoop =
  out == [42,41..1] ++ [(-1)]
  where loop = L (Loop [(S PutByte), (B Dec)])  -- print and dec
        -- start with pointer -> 42
        -- loop printing and decrementing
        -- dec and print once more when out of the loop
        program = replicate 42 (B Inc) ++ [loop] ++ [(B Dec), (S PutByte)]
        res = execState (eval simulator program) (emptyState 0)
        out = simStateOutput res

properties :: [(String, Prop)]
properties =
  [ ("eval empty program", Prop prop_EmptyProgram)
   ,("eval incs and decs", Prop prop_IncrementsDecrements)
   ,("eval simple inc and shift program", Prop prop_simpleProgram)
   ,("eval put byte", Prop prop_PutByte)
   ,("eval get byte", Prop prop_GetByte)
   ,("eval simple decrementing loop", Prop prop_DecLoop)
  ]


