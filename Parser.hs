module Parser (readTRS, readTRSFile) where

import Data.List
import Text.ParserCombinators.Parsec
import TRS

alphaNumBar = try alphaNum <|> char '_'

lowerWord :: Parser String
lowerWord = do
  spaces
  c <- lower
  s <- many alphaNumBar
  spaces
  return (c : s)

upperWord :: Parser String
upperWord = do
  spaces
  c <- upper
  s <- many alphaNumBar
  spaces
  return (c : s)

keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces
  return ()

parseParen :: Parser a -> Parser a
parseParen p = do
  keyword "("
  x <- p
  keyword ")"
  return x

parseVariable :: Parser Term
parseVariable = do
  x <- lowerWord
  return (Var x)

parseConstant :: Parser Term
parseConstant = do
  c <- upperWord
  return (Con c)

parseSimpleTerm :: Parser Term
parseSimpleTerm =
  try parseVariable <|>
  try parseConstant <|>
  try (parseParen parseTerm)

parseTerm :: Parser Term
parseTerm = do
  ts <- sepBy1 parseSimpleTerm spaces
  return (foldl1 App ts)

parseRule :: Parser (Term, Term)
parseRule = do
  l <- parseTerm
  keyword "="
  r <- parseTerm
  keyword "."
  case extraVariables l r of
    [] -> return (l, r)
    x : _ -> fail ("Unknown variable: " ++ x)

parseTRS :: Parser TRS
parseTRS = do
  trs <- many parseRule
  eof
  return trs

variables :: Term -> [String]
variables (Con _)   = []
variables (Var x)   = [x]
variables (App t u) = nub (variables t ++ variables u)

extraVariables :: Term -> Term -> [String]
extraVariables l r = nub [ x | x <- variables r, not (elem x (variables l)) ]

readTRSFile :: String -> IO (Either ParseError TRS)
readTRSFile path = parseFromFile parseTRS path

readTRS :: String -> Either ParseError TRS
readTRS s = parse parseTRS "(direct input)" s
