module TRS where

data Term = Var String | Con String | App Term Term
  deriving (Eq, Show)

type TRS = [(Term, Term)]
