module HSCalc (hsCalc, prettyString, CalcError (..)) where

import Data.Bifunctor (Bifunctor (first))
import Evaluator (EvaluateError (..), evaluate)
import Lexer (LexicalError (..), tokenize)
import Parser (ParseError (..), parse)

data CalcError = Lexer LexicalError | Parser ParseError | Evaluate EvaluateError
  deriving (Show)

hsCalc :: String -> Either CalcError Double
hsCalc input =
  first Lexer (tokenize input)
    >>= \tokens ->
      first Parser (parse tokens)
        >>= \ast -> first Evaluate (evaluate ast)

prettyString :: CalcError -> String
prettyString err =
  "Error: "
    ++ ( case err of
           Lexer e -> case e of
             UndefinedChar c -> "Character `" ++ c : "` is undefined & invalid"
             UndefinedIdent s -> "Identifier `" ++ s ++ "` is not defined"
           Parser e -> case e of
             ExpectedNumberGotEoF -> "Expected a number but got EoF"
             ExpectedClosingParenGotEoF -> "Expected a closing parenthesis `)` to pair `(` but got EoF"
             InvalidSyntaxGot token -> "Invalid syntax, got " ++ show token
             ExpectedNumberGot token -> "Expected a number but, got " ++ show token
             ExpectedClosingParenGot token -> "Expected a closing parenthesis `)` to pair `(` but got " ++ show token
           Evaluate e -> case e of
             Undefined -> "Undefined, under development"
       )