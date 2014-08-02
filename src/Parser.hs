module Parser (
       Program
     , Op(..)
     , parseProgram
     ) where

import Text.Parsec.Prim                  ((<|>), runP, skipMany)
import Text.Parsec.Combinator            (between, sepEndBy, eof)
import Text.Parsec.Error                 (ParseError)
import Text.Parsec.ByteString.Lazy       (Parser)
import Text.Parsec.Char                  (space, spaces, char)
import Control.Applicative               ((<*))
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

simpleChar :: Parser Char
simpleChar = (char '>') <|> (char '<') <|> (char '+') <|>
             (char '-') <|> (char '.') <|> (char ',')

simpleOp :: Parser Op
simpleOp = fmap build simpleChar
  where build '>' = IncP
        build '<' = DecP
        build '+' = Inc
        build '-' = Dec
        build '.' = PutByte
        build ',' = GetByte

loop :: Parser Op
loop = fmap Loop $ between (char '[') (char ']') program

parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram s = runP fullProgram () "" s
