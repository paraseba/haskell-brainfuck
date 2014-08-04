import Eval
  ( evalBS, EvalResult(EvalSuccess,EvalExecError,EvalParseError)
  , defaultIOMachine, BFExError, Tape(Tape), BFTape, errMsg, errTape, rTape)
import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs, getProgName)
import qualified Data.ByteString.Lazy as BS

main :: IO ExitCode
main = do
  getProgram                >>=
    BS.readFile             >>=
    evalBS defaultIOMachine >>=
    reportResults           >>=
    exitWith

getProgram :: IO FilePath
getProgram = do
  args <- getArgs
  if length args /= 1
    then do
           exe <- getProgName
           error $ "Usage: " ++ exe ++ " filepath"
    else return $ head args

reportResults :: EvalResult -> IO ExitCode

reportResults (EvalSuccess _) = return ExitSuccess

reportResults (EvalParseError parseError) = do
  putStrLn $ "Error parsing program:"
  putStrLn $ show parseError
  return $ ExitFailure 1

reportResults (EvalExecError err) = do
  putStrLn $ "Error evaluating program: " ++ errMsg err
  putStrLn $ "Current tape value: " ++ (show . rTape . errTape) err
  putStrLn $ "Consumed tape: " ++ (showConsumed . errTape) err
  return $ ExitFailure 2

showConsumed :: BFTape -> String
showConsumed (Tape _ _ r) =
  "[" ++ concatMap ((++ ",") . show) (consumed r ++ [0,0,0]) ++ "..."

consumed :: (Eq a, Num a) => [a] -> [a]
consumed (0:0:0:0:0:0:0:0:0:0:_) = []
consumed (x:xs) = x : consumed xs
