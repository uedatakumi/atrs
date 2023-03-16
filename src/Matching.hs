module Matching where

import Substitution
import TRS

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
