module Test.Eval (properties) where

import Test.QuickCheck
import Debug.Trace
import Control.Monad.State
import Data.Int (Int8)
import Data.Char (chr)

import Test.Helper
import Parser
import Tape
import Eval

evaluate :: Program -> Tape Int8
evaluate p = tape $ evalState (eval simulator p) (emptyState 0)

prop_EmptyProgram :: Bool
prop_EmptyProgram = current == 0
  where (Tape _ current _) = evaluate []

prop_IncrementsDecrements :: Positive Int8 -> Positive Int8 -> Bool
prop_IncrementsDecrements (Positive incs) (Positive decs) =
  current == incs - decs
  where program = replicate (fromIntegral incs) Inc ++
                  replicate (fromIntegral decs) Dec
        (Tape _ current _) = evaluate program

prop_simpleProgram :: Bool
prop_simpleProgram =
  current == 1 && next !! 0 == 4
  where program = [Inc, Inc,             -- +2
                   IncP,Inc,Inc,Inc,Inc, -- right +4
                   DecP,Dec              -- left -1
                  ]
        res = evaluate program
        (Tape prev current next) = res

prop_PutByte :: Bool
prop_PutByte =
  out == [0, 1, 2, 1]
  where program = [PutByte, Inc, PutByte, Inc, PutByte, Dec, PutByte]
        res = execState (eval simulator program) (emptyState 0)
        out = simStateOutput res

prop_GetByte :: Bool
prop_GetByte =
  out == [0, 42]
  where program = [PutByte, Inc, GetByte, PutByte]
        res = execState (eval simulator program) (SimState [42] [])
        out = simStateOutput res

prop_DecLoop :: Bool
prop_DecLoop =
  out == [42,41..1] ++ [(-1)]
  where loop = (Loop [PutByte, Dec])  -- print and dec
        -- start with pointer -> 42
        -- loop printing and decrementing
        -- dec and print once more when out of the loop
        program = replicate 42 Inc ++ [loop] ++ [Dec, PutByte]
        res = execState (eval simulator program) (emptyState 0)
        out = simStateOutput res

prop_EvalString :: Bool
prop_EvalString =
  out == [2]
  where program = "++."
        res = execState (evalStr simulator program) (emptyState 42)
        out = simStateOutput res

-- taken from http://www.hevanet.com/cristofd/brainfuck/
squares :: [Char]
squares = "++++[>+++++<-]>[<+++++>-]+<+[\n    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+\n    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]\n    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-\n]\n"

outToString :: [Int8] -> String
outToString = map (chr . fromIntegral)

prop_Squares :: Bool
prop_Squares =
  outToString out == expected
  where program = squares
        res = execState (evalStr simulator program) (emptyState 0)
        out = simStateOutput res
        expected = concat [show (n*n) ++ "\n" | n <- [0..100]]

-- taken from http://rosettacode.org/wiki/Even_or_odd
isOddCode :: [Char]
isOddCode = " ,[>,----------]\n++<\n[->-[>+>>]>\n[+[-<+>]>+>>]\n<<<<<]\n>[-]<++++++++\n[>++++++<-]\n>[>+<-]>.\n"

digits :: Integer -> [Int8]
digits = reverse . digitsRev
  where digitsRev i = case i of
          0 -> []
          _ -> fromIntegral lastDigit : digitsRev rest
          where (rest, lastDigit) = quotRem i 10

prop_WithInput :: (Positive Integer) -> Bool
prop_WithInput (Positive n) =
  outToString out == expected
  where program = isOddCode
        res = execState (evalStr simulator program)
                        (SimState numDigits [])
        out = simStateOutput res
        numDigits = digits n ++ [fromIntegral (fromEnum '\n')]
        -- The program returns the string 1 for odd numbers and 0 for even
        expected = if odd n then "1" else "0"

properties :: [(String, Prop)]
properties =
  [ ("eval empty program", Prop prop_EmptyProgram)
   ,("eval incs and decs", Prop prop_IncrementsDecrements)
   ,("eval simple inc and shift program", Prop prop_simpleProgram)
   ,("eval put byte", Prop prop_PutByte)
   ,("eval get byte", Prop prop_GetByte)
   ,("eval simple decrementing loop", Prop prop_DecLoop)

   ,("eval string", Prop prop_EvalString)
   ,("eval squares program", Prop prop_Squares)
   ,("eval program with input", Prop prop_WithInput)
  ]
