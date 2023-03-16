module TRS where

data Term = Var String | Con String | App Term Term
  deriving Eq

type TRS = [(Term, Term)]

showTerm :: Term -> String
showTerm (App t u) = showTerm t ++ " " ++ showSimpleTerm u
showTerm t         = showSimpleTerm t

showSimpleTerm :: Term -> String
showSimpleTerm (Var x) = x
showSimpleTerm (Con c) = c
showSimpleTerm t       = "(" ++ showTerm t ++ ")"

instance Show Term where
  show t = showTerm t
