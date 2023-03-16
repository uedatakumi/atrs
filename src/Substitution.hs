module Substitution where

import TRS

type Substitution = [(String, Term)]

substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Con c) _         = Con c
substitute (App t u) sigma   = App (substitute t sigma) (substitute u sigma)
