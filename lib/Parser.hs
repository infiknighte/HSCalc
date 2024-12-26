module Parser where

import Lexer qualified as L (Operator (..), Token (..))

data AST
  = Number Double
  | UnaryOp L.Operator AST
  | BinaryOp L.Operator AST AST
  deriving (Show)

class Parsable err tok ast where
  parse :: [tok] -> Either err ast
  expr :: [tok] -> Either err (ast, [tok])
  factor :: [tok] -> Either err (ast, [tok])
  binOp :: ([tok] -> Either err (ast, [tok])) -> (L.Operator -> Bool) -> [tok] -> Either err (ast, [tok])

instance Parsable String L.Token AST where
  parse :: [L.Token] -> Either String AST
  parse tokens =
    expr tokens >>= \(ast, toks) ->
      if null toks
        then Right ast
        else Left "Invalid syntax: Unexptected end of tokens"

  expr = binOp term (`elem` [L.Add, L.Subtract])
    where
      term = binOp power (`elem` [L.Multiply, L.Divide])
        where
          power = binOp factor (== L.Exponent)

  factor [] = Left "Expected a number, got EoF"
  factor (token : tokens) = case token of
    L.Number value -> Right (Number value, tokens)
    L.OpenParen ->
      expr tokens >>= \(ast, tokens') -> case tokens' of
        L.CloseParen : tokens'' -> Right (ast, tokens'')
        _ -> Left $ "Expected an closing parenthesis ')', got EoF"
    _ -> Left $ "Expected a number, got '" ++ show token ++ "'"

  binOp func predicate toks = do
    (lhs, toks') <- func toks
    binOp' lhs toks'
    where
      binOp' :: AST -> [L.Token] -> Either String (AST, [L.Token])
      binOp' ast [] = Right (ast, [])
      binOp' ast (token : tokens) = case token of
        L.Operator opr | predicate opr -> func tokens >>= \(rhs, tokens') -> binOp' (BinaryOp opr ast rhs) tokens'
        t -> Right (ast, t : tokens)
