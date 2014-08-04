module Eval (
  eval, evalBS, evalStr
  ,EvalResult(..)
  ,Machine(Machine)
  ,putByte, getByte
  ,defaultIOMachine
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState

  -- exported from Tape
  ,BFExError, Tape(Tape), BFTape, errMsg, errTape, rTape
  -- exported from Parser
  ,ParseError
) where

import Data.Int (Int8)
import Control.Monad (liftM)
import Control.Monad.State (State, modify, state, StateT(StateT), execStateT, get, put, mapStateT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Control.Monad.Error

import Parser (ParseError)
import qualified Parser as P
import Tape (Tape(Tape), blankTape, BFTape, BFExError, errMsg, errTape, rTape, wTape, left, right, inc, dec)


type TapeState m = StateT BFTape (ErrorT BFExError m)

data Machine m = Machine { putByte :: Int8 -> m ()
                          ,getByte :: m Int8
                         }

evalTape :: Monad m
         => Machine m
         -> P.Program
         -> TapeState m ()
evalTape m = mapM_ (evalOp m)

eval :: Monad m => Machine m -> P.Program -> m (Either BFExError BFTape)
eval m p = runErrorT $ execStateT (evalTape m p) blankTape

data EvalResult = EvalSuccess BFTape
                | EvalExecError BFExError
                | EvalParseError ParseError

evalBS :: Monad m => Machine m -> BS.ByteString -> m EvalResult
evalBS machine program =
  either parseError evaluate . P.parseProgram $ program
  where parseError = return . EvalParseError
        evaluate = liftM (either EvalExecError EvalSuccess) . eval machine

evalStr :: Monad m => Machine m -> String -> m EvalResult
evalStr m = evalBS m . BSC.pack

evalOp :: Monad m
       => Machine m
       -> P.Op
       -> TapeState m ()

evalOp _ P.IncP =
  StateT f
  where f tape = case right tape of
                   (Right tape') -> ErrorT $ return $ Right ((), tape')
                   (Left e)      -> ErrorT $ return $ Left e

evalOp _ P.DecP =
  StateT f
  where f tape = case left tape of
                   (Right tape') -> ErrorT $ return $ Right ((), tape')
                   (Left e)      -> ErrorT $ return $ Left e

evalOp _ P.Inc =
  StateT f
  where f = ErrorT . return . Right . ((),) . inc

evalOp _ P.Dec =
  StateT f
  where f = ErrorT . return . Right . ((),) . dec

evalOp machine (P.Loop ops) = do
  tape <- get
  if (rTape tape == 0)
  then return ()
  else evalTape machine ops >> evalOp machine (P.Loop ops)

evalOp (Machine{putByte = putByte}) P.PutByte =
  StateT f
  where f tape =  ErrorT  $ putByte (rTape tape) >>= return . Right . (,tape)

evalOp (Machine{getByte = getByte}) P.GetByte =
  StateT f
  where f tape = ErrorT $ getByte >>= (\b -> return $ Right ((), wTape b tape))


defaultIOMachine :: Machine IO
defaultIOMachine = Machine (putChar . toEnum . fromIntegral)
                           (fmap (fromIntegral . fromEnum) getChar)

-----
data SimState = SimState {input :: [Int8], output :: [Int8]}

simStateOutput :: SimState -> [Int8]
simStateOutput = reverse . output

emptyState :: SimState
emptyState = SimState [] []

simulator :: Machine (State SimState)
simulator = Machine (\byte -> modify (writeByte byte))
                    (state readByte)
            where  writeByte byte s@(SimState{output = o}) = s{output = byte : o}
                   readByte s@(SimState{input = (byte:rest)}) = (byte, s{input = rest})

