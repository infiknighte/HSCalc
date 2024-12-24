module HSCalc (hsCalc) where

import Evaluator (Evaluatable (evaluate))
import Lexer (Lexable (tokenize), Token (..))
import Parser (AST, Parsable (parse))

hsCalc :: String -> Either String Double
hsCalc input = (tokenize input :: Either String [Token]) >>= \tokens -> (parse tokens :: Either String AST) >>= \ast -> evaluate ast

