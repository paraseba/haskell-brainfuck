module Eval (
  eval
  ,Machine
  ,tape, putByte, getByte
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState
) where

import Data.Int (Int8)
import Data.List (foldl')
import Control.Monad.State
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


evalOp :: Monad m => Op -> m (Machine m) -> m (Machine m)

evalOp (P IncP) machine = machine >>= return . update right --move the head right
evalOp (P DecP) machine = machine >>= return . update left
evalOp (B Inc)  machine = machine >>= return . update inc
evalOp (B Dec)  machine = machine >>= return . update dec

evalOp (S PutByte) machine = do
  m <- machine
  let current = (rTape . tape) m
  putByte m current
  return m

evalOp (S GetByte) machine = do
  m <- machine
  b <- getByte m
  (return . update (wTape b)) m

evalOp (L (Loop ops)) machine = do
  m <- machine
  if ((==0) . rTape . tape) m
    then return m
    else evalOp (L (Loop ops)) $ eval m ops


-----
data SimState = SimState {input :: [Int8] -> Int8, output :: [Int8]}

simStateOutput :: SimState -> [Int8]
simStateOutput = reverse . output

emptyState :: Int8 -> SimState
emptyState inputByte = SimState (const inputByte) []

simulator :: Machine (State SimState)
simulator = Machine blankTape
                    (\byte -> modify (writeByte byte))
                    readByte
            where  writeByte byte s@(SimState{output = o}) = s{output = byte : o}
                   readByte = do
                     state <- get
                     return $ input state $ output state


