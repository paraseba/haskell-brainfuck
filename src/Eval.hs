module Eval (
  eval, evalBS, evalStr
  ,Machine(Machine)
  ,putByte, getByte
  ,defaultIOMachine
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState
) where

import Debug.Trace --fixme

import Data.Int (Int8)
import Control.Monad.State (State, modify, state, StateT(StateT), execStateT, get, put, mapStateT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Control.Monad.Error

import qualified Parser as P
import Tape (BFTape, BFExError)
import qualified Tape as T

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
eval m p = runErrorT $ execStateT (evalTape m p) T.blankTape

evalBS :: Monad m => Machine m -> BS.ByteString -> m (Either BFExError BFTape)
evalBS machine program =
  case fmap (eval machine) $ P.parseProgram program of
    -- fixme
    (Right res) -> res

evalStr :: Monad m => Machine m -> String -> m (Either BFExError BFTape)
evalStr m = evalBS m . BSC.pack

evalOp :: Monad m
       => Machine m
       -> P.Op
       -> TapeState m ()

evalOp _ P.IncP =
  StateT f
  where f tape = case T.right tape of
                   (Right tape') -> ErrorT $ return $ Right ((), tape')
                   (Left e)      -> ErrorT $ return $ Left e

evalOp _ P.DecP =
  StateT f
  where f tape = case T.left tape of
                   (Right tape') -> ErrorT $ return $ Right ((), tape')
                   (Left e)      -> ErrorT $ return $ Left e

evalOp _ P.Inc =
  StateT f
  where f = ErrorT . return . Right . ((),) . T.inc

evalOp _ P.Dec =
  StateT f
  where f = ErrorT . return . Right . ((),) . T.dec

evalOp machine (P.Loop ops) = do
  tape <- get
  if (T.rTape tape == 0)
  then return ()
  else evalTape machine ops >> evalOp machine (P.Loop ops)

evalOp (Machine{putByte = putByte}) P.PutByte =
  StateT f
  where f tape =  ErrorT  $ putByte (T.rTape tape) >>= return . Right . (,tape)

evalOp (Machine{getByte = getByte}) P.GetByte =
  StateT f
  where f tape = ErrorT $ getByte >>= (\b -> return $ Right ((), T.wTape b tape))


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

