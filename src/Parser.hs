module Parser (
       Program
     , PointerOp (IncP, DecP)
     , ByteOp (Inc, Dec)
     , SideEffectOp (PutByte, GetByte)
     , Loop (Loop)
     , Op (P, B, S, L)
     , parseProgram

   ) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Control.Applicative ((<*))
import qualified Data.ByteString.Lazy as BS
--import qualified Text.Parsec.Token as Tok

data PointerOp = IncP | DecP
  deriving (Show,Eq)

data ByteOp = Inc | Dec
  deriving (Show,Eq)

data SideEffectOp = PutByte | GetByte
  deriving (Show,Eq)

data Loop = Loop [Op]
  deriving (Show,Eq)

data Op = P PointerOp | B ByteOp | S SideEffectOp | L Loop
  deriving (Show,Eq)

type Program = [Op]

program :: Parser Program
program = do
  skipMany space
  sepEndBy operation spaces

fullProgram :: Parser Program
fullProgram = program <* eof

operation :: Parser Op
operation = simpleOp <|> loop

simpleOp :: Parser Op
simpleOp = do
  c <- (char '>') <|> (char '<') <|> (char '+') <|>
       (char '-') <|> (char '.') <|> (char ',')
  return $ case c of
    '>' -> P IncP
    '<' -> P DecP
    '+' -> B Inc
    '-' -> B Dec
    '.' -> S PutByte
    ',' -> S GetByte

loop :: Parser Op
loop = do
  p <- between (char '[') (char ']') program
  return $ L (Loop p)

parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram s = runP fullProgram () "" s
