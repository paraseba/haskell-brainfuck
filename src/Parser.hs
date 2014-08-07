{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Parser
Description : Parse brainfuck program into a Program abstract datatype
Copyright   : (c) Sebastian Galkin, 2014
License     : MIT
Maintainer  : paraseba@gmail.com
Stability   : experimental

-}

module Parser (
       Program
     , Op (..)
     , parseProgram
     , module Text.Parsec.Error
     ) where

import Text.Parsec.Prim
  ( (<|>), runP, skipMany )

import Text.Parsec.Combinator
  ( between, sepEndBy, eof )

import Text.Parsec.Error
  ( ParseError )

import Text.Parsec.ByteString.Lazy
  ( Parser )

import Text.Parsec.Char
  ( space, spaces, char )

import Control.Applicative
  ( (<*), (<$>) )

import qualified Data.ByteString.Lazy as BS

-- | Brainfuck operations
data Op
  = IncP      -- ^ Increment the byte pointer: @ ">" @
  | DecP      -- ^ Decrement the byte pointer: @ "<" @
  | Inc       -- ^ Increment the byte pointed by the pointer: @ "+" @
  | Dec       -- ^ Decrement the byte pointed by the pointer: @ "-" @
  | PutByte   -- ^ Write the byte pointed by the pointer (side-effect): @ "." @
  | GetByte   -- ^ Read a byte, write at the current location
              -- (side-effect): @ "," @
  | Loop [Op] -- ^ Loop over 'Op's untill current @ == 0 @: @ "[...]" @
  deriving (Show, Eq)

-- | A program is a list of 'Op's. One possible 'Op' is a 'Loop' of other 'Op's
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
        build _   = error "Unknown character" -- this should never happen

loop :: Parser Op
loop = Loop <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram = runP fullProgram () ""
