module TRS where

data Term = Var String | Con String | App Term Term
  deriving Eq

type TRS = [(Term, Term)]

showTerm :: Term -> String
showTerm (Var x)   = x
showTerm (Con c)   = c
showTerm (App t u) = "(" ++ showTerm t ++ " " ++ showTerm u ++ ")"

instance Show Term where
  show t = showTerm t
