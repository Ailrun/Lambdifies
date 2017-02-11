module Lambdifies.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Lambdifies.Type
import Lambdifies.Abbreviation


exprK :: Parser Expr
exprK =
  do
    _ <- char 'K'
    return k

exprS :: Parser Expr
exprS =
  do
    _ <- char 'S'
    return s

exprN :: Parser Expr
exprN =
  do
    _ <- char 'N'
    natTy <- oneOf "gp"
    return $
      case natTy of
        'g' -> g
        'p' -> p

exprI :: Parser Expr
exprI =
  do
    _ <- char 'I'
    return i

exprB :: Parser Expr
exprB =
  do
    _ <- char 'B'
    return b

exprY :: Parser Expr
exprY =
  do
    _ <- char 'Y'
    return y

exprAtom :: Parser Expr
exprAtom =
  exprK <|>
  exprS <|>
  exprN <|>
  exprI <|>
  exprB <|>
  exprY <|>
  do
    _ <- char '('
    e <- expr
    _ <- char ')'
    return e

expr :: Parser Expr
expr =
  chainl1 exprAtom (return ExprApp)



parseExpr :: String -> IO Expr
parseExpr s =
  case parse expr "" s of
    Left err -> error (show err)
    Right e -> return e
