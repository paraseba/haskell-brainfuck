{- |
Module      : Main
Description : Evaluate a BrainFuck program
Copyright   : (c) Sebastian Galkin, 2014
License     : MIT
Maintainer  : paraseba@gmail.com
Stability   : experimental

Usage:

> brainfuck path/to/program.bf

It will parse the program and evaluate it by doing I/O to the console. In case
of parsing or execution errors it reports them to stderr

-}

module Main(main) where

import HaskBF.Eval
  ( evalBS, EvalResult ( EvalSuccess, EvalExecError, EvalParseError )
  , defaultIOMachine, BFExError, Tape ( Tape ), BFTape, errMsg, errTape, rTape )

import System.Exit
  ( ExitCode (..), exitWith )

import System.Environment
  ( getArgs, getProgName )

import qualified Data.ByteString.Lazy as BS

main :: IO ExitCode
main =
  getProgram                >>=
    BS.readFile             >>=
    evalBS defaultIOMachine >>=
    reportResults           >>=
    exitWith

{- | Read command line and obtain the path to the program. Display error message
 - if missing argument -}
getProgram :: IO FilePath
getProgram = do
  args <- getArgs
  if length args /= 1
    then do
           exe <- getProgName
           error $ "Usage: " ++ exe ++ " filepath"
    else return $ head args

-- | Report result of program parsing and evaluation
reportResults :: EvalResult -> IO ExitCode

reportResults (EvalSuccess _) = return ExitSuccess

reportResults (EvalParseError parseError) = do
  putStrLn "Error parsing program:"
  print parseError
  return $ ExitFailure 1

reportResults (EvalExecError err) = do
  putStrLn $ "Error evaluating program: " ++ errMsg err
  putStrLn $ "Current tape value: " ++ (show . rTape . errTape) err
  putStrLn $ "Consumed tape: " ++ (showConsumed . errTape) err
  return $ ExitFailure 2

{- | Use heuristic to display tape state. It estimates consumed right tape by
 - calling 'consumed' -}
showConsumed :: BFTape -> String
showConsumed (Tape _ _ r) =
  "[" ++ concatMap ( (++ ",") . show ) (consumed r ++ [0, 0, 0]) ++ "..."

-- | Return consumed tape by assuming that it is unused after 10 zeros.
consumed :: (Eq a, Num a) => [a] -> [a]
consumed (0 : 0 : 0 : 0 : 0 : 0 : 0 : 0 : 0 : 0 : _) = []
consumed (x : xs) = x : consumed xs
