module Eval (
  eval, evalBS, evalStr
  ,Machine
  ,tape, putByte, getByte
  ,defaultIOMachine
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState
) where

import Data.Int (Int8)
import Data.List (foldl')
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Debug.Trace

import Parser
import Tape

type Putter m = Int8 -> m ()
type Getter m = m Int8

data Machine m = Machine { tape :: Tape
                          ,putByte :: Putter m
                          ,getByte :: Getter m
                         }

update :: (Tape -> Tape) -> Machine m -> Machine m
update f m = m{tape = f $ tape m}

eval :: Monad m => Machine m -> Program -> m (Machine m)
eval machine program =
  foldl' (flip evalOp) (return machine) program

evalBS :: Monad m => Machine m -> BS.ByteString -> m (Machine m)
evalBS machine program =
  case fmap (eval machine) $ parseProgram program of
    -- fixme
    (Right res) -> res

evalStr :: Monad m => Machine m -> String -> m (Machine m)
evalStr m = evalBS m . BSC.pack

evalOp :: Monad m => Op -> m (Machine m) -> m (Machine m)

evalOp IncP machine = machine >>= return . update right --move the head right
evalOp DecP machine = machine >>= return . update left
evalOp Inc  machine = machine >>= return . update inc
evalOp Dec  machine = machine >>= return . update dec

evalOp PutByte machine = do
  m <- machine
  let current = (rTape . tape) m
  putByte m current
  return m

evalOp GetByte machine = do
  m <- machine
  b <- getByte m
  (return . update (wTape b)) m

evalOp (Loop ops) machine = do
  m <- machine
  if ((==0) . rTape . tape) m
    then return m
    else evalOp (Loop ops) $ eval m ops


defaultIOMachine :: Machine IO
defaultIOMachine = Machine blankTape
                           (putChar . toEnum . fromIntegral)
                           (fmap (fromIntegral . fromEnum) getChar)

-----
data SimState = SimState {input :: [Int8], output :: [Int8]}

simStateOutput :: SimState -> [Int8]
simStateOutput = reverse . output

emptyState :: Int8 -> SimState
emptyState inputByte = SimState [] []

simulator :: Machine (State SimState)
simulator = Machine blankTape
                    (\byte -> modify (writeByte byte))
                    (state readByte)
            where  writeByte byte s@(SimState{output = o}) = s{output = byte : o}
                   readByte s@(SimState{input = (byte:rest)}) = (byte, s{input = rest})

