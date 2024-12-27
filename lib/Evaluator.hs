module Evaluator (evaluate, EvaluateError) where

import Data.Fixed (mod')
import Lexer qualified as L (Operator (..))
import Parser qualified as P (AST (..))

data EvaluateError = Undefined
  deriving (Show)

evaluate :: P.AST -> Either EvaluateError Double
evaluate = evaluate' 0
  where
    eval = evaluate' 0
    evaluate' :: Double -> P.AST -> Either EvaluateError Double
    evaluate' acc ast = case ast of
      P.Number value -> Right $ acc + value
      P.BinaryOp opr lhs rhs ->
        evalBinOp
          lhs
          rhs
          ( case opr of
              L.Add -> (+)
              L.Subtract -> (-)
              L.Multiply -> (*)
              L.Divide -> (/)
              L.Modulus -> mod'
              L.Exponent -> (**)
              _ -> undefined
          )
      _ -> Left Undefined
      where
        evalBinOp lhs rhs opr = eval lhs >>= \l -> eval rhs >>= \r -> Right $ acc + (l `opr` r)