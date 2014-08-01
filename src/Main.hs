import Eval (evalBS, defaultIOMachine)
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

main :: IO ()
main = do
  path <- getProgram
  code <- BS.readFile path
  evalBS defaultIOMachine code
  return ()
