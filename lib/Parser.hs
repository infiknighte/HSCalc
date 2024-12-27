module Parser where

import Lexer qualified as L (Operator (..), Token (..))

data AST
  = Number Double
  | UnaryOp L.Operator AST
  | BinaryOp L.Operator AST AST
  deriving (Show)

data ParseError
  = ExpectedNumberGotEoF
  | ExpectedClosingParenGotEoF
  | InvalidSyntaxGot L.Token
  | ExpectedNumberGot L.Token
  | ExpectedClosingParenGot L.Token
  deriving (Show)

parse :: [L.Token] -> Either ParseError AST
parse tokens =
  expr tokens >>= \(ast, toks) ->
    if null toks
      then Right ast
      else Left . InvalidSyntaxGot $ head toks

expr :: [L.Token] -> Either ParseError (AST, [L.Token])
expr = binOp term (`elem` [L.Add, L.Subtract])
  where
    term = binOp power (`elem` [L.Multiply, L.Divide])
      where
        power = binOp factor (== L.Exponent)

factor :: [L.Token] -> Either ParseError (AST, [L.Token])
factor [] = Left ExpectedNumberGotEoF
factor (token : tokens) = case token of
  L.Number value -> Right (Number value, tokens)
  L.OpenParen ->
    expr tokens >>= \(ast, tokens') -> case tokens' of
      L.CloseParen : tokens'' -> Right (ast, tokens'')
      t : _ -> Left $ ExpectedClosingParenGot t
      [] -> Left ExpectedClosingParenGotEoF
  t -> Left $ ExpectedNumberGot t

binOp :: ([L.Token] -> Either ParseError (AST, [L.Token])) -> (L.Operator -> Bool) -> [L.Token] -> Either ParseError (AST, [L.Token])
binOp f p toks = do
  (lhs, toks') <- f toks
  binOp' lhs toks'
  where
    binOp' :: AST -> [L.Token] -> Either ParseError (AST, [L.Token])
    binOp' ast [] = Right (ast, [])
    binOp' ast (token : tokens) = case token of
      L.Operator opr | p opr -> f tokens >>= \(rhs, toks') -> binOp' (BinaryOp opr ast rhs) toks'
      t -> Right (ast, t : tokens)
