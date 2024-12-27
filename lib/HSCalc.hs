module HSCalc (hsCalc) where

import Data.Bifunctor (Bifunctor (first))
import Evaluator (EvaluateError, evaluate)
import Lexer (LexicalError, tokenize)
import Parser (ParseError, parse)

data CalcError = Lexer LexicalError | Parser ParseError | Evaluate EvaluateError
  deriving (Show)

hsCalc :: String -> Either CalcError Double
hsCalc input =
  first Lexer (tokenize input)
    >>= \tokens ->
      first Parser (parse tokens)
        >>= \ast -> first Evaluate (evaluate ast)
