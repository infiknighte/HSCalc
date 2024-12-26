module Evaluator (Evaluatable (evaluate)) where

import Data.Fixed (mod')
import Lexer qualified as L (Operator (..))
import Parser qualified as P (AST (..))

class Evaluatable input error output where
  evaluate :: input -> Either error output

instance Evaluatable P.AST String Double where
  evaluate = evaluate' 0
    where
      eval = evaluate' 0
      evaluate' :: Double -> P.AST -> Either String Double
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
        _ -> Left "Undefined AST"
        where
          evalBinOp lhs rhs opr = eval lhs >>= \l -> eval rhs >>= \r -> Right $ acc + (l `opr` r)