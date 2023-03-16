import Matching
import TRS
import Parser
import Substitution
import System.Environment

rewrite' :: TRS -> Term -> Maybe Term
rewrite' [] _               = Nothing
rewrite' ((l, r) : rs) t
  | Just sigma <- match l t = Just (substitute r sigma)
  | otherwise               = rewrite' rs t

rewrite :: TRS -> Term -> Maybe Term
rewrite trs (Var x)          = rewrite' trs (Var x)
rewrite trs (Con c)          = rewrite' trs (Con c)
rewrite trs (App t1 t2)
  | Just u <- rewrite trs t2 = Just (App t1 u)
  | Just u <- rewrite trs t1 = Just (App u t2)
  | otherwise                = rewrite' trs (App t1 t2)

nf :: TRS -> Term -> Term
nf trs t
  | Just u <- rewrite trs t = nf trs u
  | otherwise               = t

main :: IO()
main = do
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> putStrLn (show (nf trs (Con "MAIN")))
