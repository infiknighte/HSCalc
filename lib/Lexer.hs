module Lexer (tokenize, Token (..), Operator (..), LexicalError (..), isOperator) where

import Data.Bifunctor as BF (first)
import Data.Char (isAlpha, isNumber, isSpace)
import Text.Read (readMaybe)

data Token = Number Double | Operator Operator | OpenParen | CloseParen
  deriving (Show, Eq)

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Percent
  | Modulus
  | SquareRoot
  | Exponent
  deriving (Show, Eq)

instance Read Operator where
  readsPrec _ str = case str of
    "+" -> [(Add, "")]
    "-" -> [(Subtract, "")]
    "*" -> [(Multiply, "")]
    "/" -> [(Divide, "")]
    "^" -> [(Exponent, "")]
    "%" -> [(Percent, "")]
    "mod" -> [(Modulus, "")]
    "sqrt" -> [(SquareRoot, "")]
    _ -> []

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^%"

tokenize :: String -> Either LexicalError [Token]
tokenize = tokenize' []
  where
    tokenize' acc src = case nextToken src of
      Left err -> Left err
      Right Nothing -> Right $ reverse acc
      Right (Just (token, rest)) -> tokenize' (token : acc) rest

constants :: [String]
constants = ["PI"]

constantValue :: String -> Double
constantValue = \case
  "PI" -> pi
  _ -> undefined

data LexicalError = UndefinedChar Char | UndefinedIdent String
  deriving (Show)

nextToken :: String -> Either LexicalError (Maybe (Token, String))
nextToken src =
  if null trimmed
    then Right Nothing
    else case head trimmed of
      '(' -> Right . Just $ (OpenParen, tail trimmed)
      ')' -> Right . Just $ (CloseParen, tail trimmed)
      c | isOperator c -> Right . Just $ (Operator $ read [c], tail trimmed)
      c
        | isAlpha c ->
            let (str, rest) = makeIdentifier trimmed
             in case readMaybe str of
                  Just opr -> Right . Just $ (Operator opr, rest)
                  Nothing -> case str of
                    s | s `elem` constants -> Right . Just $ (Number $ constantValue s, rest)
                    s -> Left $ UndefinedIdent s
      c
        | isNumber c || c == '.' ->
            let (num, rest) = makeNumber trimmed in Right . Just $ (Number num, rest)
      c -> Left $ UndefinedChar c
  where
    trimmed = dropWhile isSpace src

makeIdentifier :: String -> (String, String)
makeIdentifier "" = ("", "")
makeIdentifier str = makeIdentifier' [] str
  where
    makeIdentifier' :: String -> String -> (String, String)
    makeIdentifier' acc src = case src of
      "" -> (acc, "")
      c : cs ->
        if isAlpha c || c `elem` "_"
          then makeIdentifier' (acc ++ [c]) cs
          else (acc, src)

makeNumber :: String -> (Double, String)
makeNumber "" = (0, "")
makeNumber str = BF.first read $ makeNumber' [] str False
  where
    makeNumber' :: String -> String -> Bool -> (String, String)
    makeNumber' acc src dots = case src of
      "" -> (putZero acc, "")
      c : cs ->
        if c == '.' && dots
          then
            (putZero acc, src)
          else
            if c `elem` "0123456789."
              then makeNumber' (acc ++ [c]) cs (c == '.')
              else (putZero acc, src)
      where
        putZero :: String -> String
        putZero "" = ""
        putZero s =
          let s' =
                if head s == '.'
                  then '0' : s
                  else s
           in if last s' == '.'
                then s' ++ "0"
                else s'