import Eval (evalBS, defaultIOMachine)
import Tape (BFExError, Tape(Tape), BFTape, errMsg, errTape, rTape)
import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs, getProgName)
import qualified Data.ByteString.Lazy as BS

getProgram :: IO FilePath
getProgram = do
  args <- getArgs
  if length args /= 1
    then do
           exe <- getProgName
           error $ "Usage: " ++ exe ++ " filepath"
    else return $ head args

main :: IO ExitCode
main = do
  path <- getProgram
  code <- BS.readFile path
  res  <- evalBS defaultIOMachine code
  code <- report res
  exitWith code

report :: Either BFExError BFTape -> IO ExitCode
report (Right tape) = return ExitSuccess
report (Left err) = do
  putStrLn $ "Error evaluating program: " ++ errMsg err
  putStrLn $ "Current value: " ++ (show . rTape . errTape) err
  putStrLn $ "Consumed tape: " ++ (showConsumed . errTape) err
  return $ ExitFailure 1

showConsumed :: BFTape -> String
showConsumed (Tape _ _ r) =
  "[" ++ concatMap ((++ ",") . show) (consumed r ++ [0,0,0]) ++ "..."

consumed :: (Eq a, Num a) => [a] -> [a]
consumed (0:0:0:0:0:0:0:0:0:0:_) = []
consumed (x:xs) = x : consumed xs
