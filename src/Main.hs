import Parser
import Rewriting
import System.Environment
import TRS

main :: IO()
main = do
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> putStrLn (show (nf trs (Con "MAIN")))
