import TRS
import Parser
import System.Environment

type Substitution = [(String, Term)]

substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Con c) _         = Con c
substitute (App t u) sigma   = App (substitute t sigma) (substitute u sigma)

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma []                             = Just sigma
match' sigma ((Var x, t) : us)
  | Just t' <- lookup x sigma, t == t'      = match' sigma us
  | Nothing <- lookup x sigma               = match' ((x, t) : sigma) us
match' sigma ((Con c, Con d) : us)
  | c == d                                  = match' sigma us
match' sigma ((App t1 t2, App u1 u2) : tus) = match' sigma ((t1, u1) : (t2, u2) : tus)
match' _ _                                  = Nothing

match :: Term -> Term -> Maybe Substitution
match t u = match' [] [(t, u)]

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
