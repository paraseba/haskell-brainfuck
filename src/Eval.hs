module Eval (
  eval, evalBS, evalStr
  ,Machine(Machine)
  ,tape, putByte, getByte
  ,defaultIOMachine
  ,simulator, SimState (SimState), simStateOutput
  ,emptyState
) where

import Data.Int (Int8)
import Data.List (foldl')
import Control.Monad.State (State, modify, state)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Parser as P
import qualified Tape   as T

type Putter m = Int8 -> m ()
type Getter m = m Int8
type BFTape = T.Tape Int8

data Machine m = Machine { tape :: BFTape
                          ,putByte :: Putter m
                          ,getByte :: Getter m
                         }

update :: (BFTape -> BFTape) -> Machine m -> Machine m
update f m = m{tape = f $ tape m}

eval :: Monad m => Machine m -> P.Program -> m (Machine m)
eval machine program =
  foldl' (flip evalOp) (return machine) program

evalBS :: Monad m => Machine m -> BS.ByteString -> m (Machine m)
evalBS machine program =
  case fmap (eval machine) $ P.parseProgram program of
    -- fixme
    (Right res) -> res

evalStr :: Monad m => Machine m -> String -> m (Machine m)
evalStr m = evalBS m . BSC.pack

evalOp :: Monad m => P.Op -> m (Machine m) -> m (Machine m)

evalOp P.IncP machine = machine >>= return . update T.right --move the head right
evalOp P.DecP machine = machine >>= return . update T.left
evalOp P.Inc  machine = machine >>= return . update T.inc
evalOp P.Dec  machine = machine >>= return . update T.dec

evalOp P.PutByte machine = do
  m <- machine
  let current = (T.rTape . tape) m
  putByte m current
  return m

evalOp P.GetByte machine = do
  m <- machine
  b <- getByte m
  (return . update (T.wTape b)) m

evalOp (P.Loop ops) machine = do
  m <- machine
  if ((==0) . T.rTape . tape) m
    then return m
    else evalOp (P.Loop ops) $ eval m ops


defaultIOMachine :: Machine IO
defaultIOMachine = Machine T.blankTape
                           (putChar . toEnum . fromIntegral)
                           (fmap (fromIntegral . fromEnum) getChar)

-----
data SimState = SimState {input :: [Int8], output :: [Int8]}

simStateOutput :: SimState -> [Int8]
simStateOutput = reverse . output

emptyState :: Int8 -> SimState
emptyState inputByte = SimState [] []

simulator :: Machine (State SimState)
simulator = Machine T.blankTape
                    (\byte -> modify (writeByte byte))
                    (state readByte)
            where  writeByte byte s@(SimState{output = o}) = s{output = byte : o}
                   readByte s@(SimState{input = (byte:rest)}) = (byte, s{input = rest})

