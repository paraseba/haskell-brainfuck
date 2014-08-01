module Parser (
       Program
     , Op(..)
     , parseProgram

   ) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Control.Applicative ((<*))
import qualified Data.ByteString.Lazy as BS

data Op = IncP | DecP | Inc | Dec | PutByte | GetByte | Loop [Op]
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
    '>' -> IncP
    '<' -> DecP
    '+' -> Inc
    '-' -> Dec
    '.' -> PutByte
    ',' -> GetByte

loop :: Parser Op
loop = do
  p <- between (char '[') (char ']') program
  return $ Loop p

parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram s = runP fullProgram () "" s
