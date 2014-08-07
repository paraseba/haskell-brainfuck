{- |
Module      : Eval
Description : Evaluate a BrainFuck program
Copyright   : (c) Sebastian Galkin, 2014
License     : MIT
Maintainer  : paraseba@gmail.com
Stability   : experimental

This module exports functions that allow to evaluate a BrainFuck program.
Evaluation supports two types of error, parsing and execution, by returning
instances of 'EvalResult'

This module should be all library users need to import.
-}

module Eval (
    eval, evalBS, evalStr
  , EvalResult (..)
  , Machine (..)
  , defaultIOMachine
  , simulatorMachine, SimState (SimState), simStateOutput
  , emptyState

  , module Tape
  , module Parser
) where

import Data.Int
  ( Int8 )

import Control.Monad
  ( liftM, when )

import Control.Monad.State
  ( State, modify, state, StateT ( StateT )
  , execStateT, get, put, mapStateT )

import Control.Monad.Error
  ( ErrorT ( ErrorT ), runErrorT )

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Parser as P

import Parser
  ( ParseError )

import Tape
  ( Tape ( Tape ), blankTape, BFTape, BFExError, errMsg
  , errTape, rTape, wTape, left, right, inc, dec)

type ExecutionState m = StateT BFTape (ErrorT BFExError m)

{- | Underlying input output for the evaluation machine. Changing the monad 'm'
 - achives different results. For instance using the 'IO' monad an evaluator can
 - be created that does input/output to stdin/stdout. If the monad is 'State',
 - for instance, input/output can happen in memory.
 -
 - We offer two implementations of 'Machine':
 -
 -   * 'defaultIOMachine': under the 'IO' monad, does input/output using the
 -   standard streams
 -   * 'simulatorMachine': under the 'State' monad, does input/output on lists
 -
 - It's easy to create other 'Machine's by using different monads and
 - functions. -}
data Machine m =
  Machine { putByte :: Int8 -> m () -- ^ Write a byte to the output, under
                                    -- monad 'm'
          , getByte :: m Int8       -- ^ Get a byte under the 'm' monad
          }

evalTape :: Monad m
         => Machine m
         -> P.Program
         -> ExecutionState m ()
evalTape m = mapM_ (evalOp m)


{- | Evaluate a parsed BrainFuck program using I/O provided by the given
 - 'Machine'
 -
 - The result is either an execution error or the 'BFTape' representing the
 - resulting state of the tape after the last instruction was executed. -}
eval :: Monad m
     => Machine m                    -- ^ The machine used to do I/O
     -> P.Program                    -- ^ The program to evaluate
     -> m (Either BFExError BFTape)  -- ^ Resulting tape after evaluation or
                                     -- execution error
eval m p = runErrorT $ execStateT (evalTape m p) blankTape

-- | Evaluation result of an unparsed BrainFuck program
data EvalResult = EvalSuccess BFTape
                  {- ^ Parsing and evaluation were successful. The resulting
                   - state of the tape after the last instruction
                   - was executed. -}

                | EvalExecError BFExError
                  {- ^ The program was parsed successfully but evaluation
                   - failed.
                   - The reason for failure is overflowing a limit of the tape.
                   - The state of the tape before the error is included -}

                | EvalParseError ParseError
                  {- ^ The program can not be parsed. Parsing error message is
                   - included -}

{- | Evaluate an unparsed BrainFuck program using I/O provided by the given
 - 'Machine'
 -
 - The result is returned as an 'EvalResult' -}
evalBS :: Monad m
       => Machine m     -- ^ The machine used to do I/O
       -> BS.ByteString -- ^ The code to evaluate
       -> m EvalResult  -- ^ Parsing/evaluation result
evalBS machine program =
  either parseError evaluate . P.parseProgram $ program
  where parseError = return . EvalParseError
        evaluate = liftM (either EvalExecError EvalSuccess) . eval machine

{- | Evaluate an unparsed BrainFuck program using I/O provided by the given
 - 'Machine'
 -
 - The result is returned as an 'EvalResult' -}
evalStr :: Monad m
        => Machine m    -- ^ The machine used to do I/O
        -> String       -- ^ The code to evaluate
        -> m EvalResult -- ^ Parsing/evaluation result
evalStr m = evalBS m . BSC.pack

evolve :: Monad m
          => (BFTape -> m (Either BFExError BFTape))
          -> ExecutionState m ()
evolve g = StateT $ ErrorT . (>>= return . liftM ((),)) . g

evalOp :: Monad m
       => Machine m
       -> P.Op
       -> ExecutionState m ()

evalOp _ P.IncP = evolve $ return . right
evalOp _ P.DecP = evolve $ return . left
evalOp _ P.Inc  = evolve $ return . Right . inc
evalOp _ P.Dec  = evolve $ return . Right . dec

evalOp ( Machine { putByte = putByte } ) P.PutByte =
  evolve $ \ tape -> liftM ( const ( Right tape ) ) $ ( putByte . rTape ) tape

evalOp ( Machine {getByte = getByte } ) P.GetByte =
  evolve $ \ tape -> liftM ( Right . flip wTape tape ) getByte

evalOp machine (P.Loop ops) = do
  tape <- get
  when (rTape tape /= 0) $
    evalTape machine ops >> evalOp machine (P.Loop ops)


{- | A 'Machine' that can evaluate code under the 'IO' monad by doing I/O
 - to stdin/stout.
 -
 - Bytes are read by comnverting them from the ASCII code -}
defaultIOMachine :: Machine IO
defaultIOMachine = Machine (putChar . toEnum . fromIntegral)
                           (fmap (fromIntegral . fromEnum) getChar)

{- | State used by 'simulatorMachine' to evaluate code under the 'State' monad
 -
 - It maintains input and output bytes inside lists -}
data SimState =
  SimState {input :: [Int8], -- ^ Input bytes to use when the program
                             -- does @ "," @
            output :: [Int8] -- ^ Store for the program outputs done with
                             -- @ "." @
           }

-- | Extract the output stream from a 'simulatorMachine' state
simStateOutput :: SimState -> [Int8]
simStateOutput = reverse . output

-- | Initial 'simulatorMachine' state
emptyState :: SimState
emptyState = SimState [] []

{- | A 'Machine' that can evaluate program doing in-memory I/O under the 'State'
 - monad. It stores state as 'SimState' -}
simulatorMachine :: Machine (State SimState)
simulatorMachine =
  Machine (modify . writeByte)
          (state readByte)
  where writeByte byte s@(SimState {output = o} ) = s {output = byte : o}
        readByte s@(SimState {input = (byte : rest)}) = (byte, s {input = rest})

