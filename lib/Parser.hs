module Parser where

import Lexer qualified as L (Operator (..), Token (..))

data AST
  = Number Double
  | UnaryOp L.Operator AST
  | BinaryOp L.Operator AST AST
  deriving (Show)

class Parsable err tok ast where
  expr :: [tok] -> Either err (ast, [tok])
  term :: [tok] -> Either err (ast, [tok])
  power :: [tok] -> Either err (ast, [tok])
  factor :: [tok] -> Either err (ast, [tok])

  parse :: [tok] -> Either err ast
  parse toks = expr toks >>= Right . fst

instance Parsable String L.Token AST where
  expr :: [L.Token] -> Either String (AST, [L.Token])
  expr toks = do
    (lhs, toks') <- term toks
    expr' lhs toks'
    where
      expr' :: AST -> [L.Token] -> Either String (AST, [L.Token])
      expr' ast [] = Right (ast, [])
      expr' ast (token : tokens) = case token of
        L.Operator opr | opr == L.Add || opr == L.Subtract -> term tokens >>= \(rhs, tokens') -> expr' (BinaryOp opr ast rhs) tokens'
        t -> Right (ast, t : tokens)

  term :: [L.Token] -> Either String (AST, [L.Token])
  term toks = do
    (lhs, toks') <- power toks
    term' lhs toks'
    where
      term' :: AST -> [L.Token] -> Either String (AST, [L.Token])
      term' ast [] = Right (ast, [])
      term' ast (token : tokens) = case token of
        L.Operator opr
          | opr == L.Multiply || opr == L.Divide || opr == L.Modulus ->
              power tokens >>= \(rhs, tokens') -> term' (BinaryOp opr ast rhs) tokens'
        t -> Right (ast, t : tokens)

  power :: [L.Token] -> Either String (AST, [L.Token])
  power toks = do
    (lhs, toks') <- factor toks
    power' lhs toks'
    where
      power' :: AST -> [L.Token] -> Either String (AST, [L.Token])
      power' ast [] = Right (ast, [])
      power' ast (token : tokens) = case token of
        L.Operator L.Exponent -> factor tokens >>= \(rhs, tokens') -> power' (BinaryOp L.Exponent ast rhs) tokens'
        t -> Right (ast, t : tokens)

  factor :: [L.Token] -> Either String (AST, [L.Token])
  factor [] = Left "Expected a number, got EoF"
  factor (token : tokens) = case token of
    L.Number value -> Right (Number value, tokens)
    L.OpenParen ->
      expr tokens >>= \(ast, tokens') -> case tokens' of
        L.CloseParen : tokens'' -> Right (ast, tokens'')
        t -> Left $ "expected an closing parenthesis ')', got '" ++ show t ++ "'"
    _ -> Left $ "Factorizing, expected a number, got '" ++ show token ++ "'"
