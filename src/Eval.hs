module Eval (
  eval, evalBS, evalStr
  ,Machine(Machine)
  ,putByte, getByte
  ,defaultIOMachine
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState
) where

import Data.Int (Int8)
import Control.Monad.State (State, modify, state, StateT(StateT), execStateT, get)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Parser as P
import qualified Tape   as T

type BFTape = T.Tape Int8

data Machine m = Machine { putByte :: Int8 -> m ()
                          ,getByte :: m Int8
                         }

type TapeState m a = StateT BFTape m a

evalTape :: Monad m => Machine m -> P.Program -> TapeState m ()
evalTape m = mapM_ (evalOp m)

eval :: Monad m => Machine m -> P.Program -> m BFTape
eval m p = execStateT (evalTape m p) T.blankTape

evalBS :: Monad m => Machine m -> BS.ByteString -> m BFTape
evalBS machine program =
  case fmap (eval machine) $ P.parseProgram program of
    -- fixme
    (Right res) -> res

evalStr :: Monad m => Machine m -> String -> m BFTape
evalStr m = evalBS m . BSC.pack

evalOp :: Monad m => Machine m -> P.Op -> TapeState m ()

evalOp _ P.IncP = modify T.right
evalOp _ P.DecP = modify T.left
evalOp _ P.Inc  = modify T.inc
evalOp _ P.Dec  = modify T.dec

evalOp machine (P.Loop ops) = do
  tape <- get
  if (T.rTape tape == 0)
  then return ()
  else evalTape machine ops >> evalOp machine (P.Loop ops)

evalOp (Machine{putByte = putByte}) P.PutByte =
  StateT f
  where f tape = putByte (T.rTape tape) >>= return . (,tape)

evalOp (Machine{getByte = getByte}) P.GetByte =
  StateT f
  where f tape = getByte >>= (\b -> return ((), T.wTape b tape))


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

