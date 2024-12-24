module Lexer (Lexable (tokenize), Token (..), Operator (..), isOperator) where

import Data.Char (isAlpha, isNumber)

data Token = Number Double | Operator Operator | OpenParen | CloseParen
  deriving (Show, Eq)

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Exponent
  | Modulus
  deriving (Show, Eq)

instance Read Operator where
  readsPrec _ str = case str of
    "+" -> [(Add, "")]
    "-" -> [(Subtract, "")]
    "*" -> [(Multiply, "")]
    "/" -> [(Divide, "")]
    "^" -> [(Exponent, "")]
    "%" -> [(Modulus, "")]
    "mod" -> [(Modulus, "")]
    _ -> []

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^%"

class Lexable src err tok where
  nextToken :: src -> Either err (Maybe (tok, src))
  tokenize :: src -> Either err [tok]
  tokenize = tokenize' []
    where
      tokenize' acc src = case nextToken src of
        Left err -> Left err
        Right Nothing -> Right $ reverse acc
        Right (Just (token, rest)) -> tokenize' (token : acc) rest

instance Lexable String String Token where
  nextToken src =
    if null trimmed
      then Right Nothing
      else case head trimmed of
        '(' -> Right . Just $ (OpenParen, tail trimmed)
        ')' -> Right . Just $ (CloseParen, tail trimmed)
        c | isOperator c -> Right . Just $ (Operator (read [c]), tail trimmed)
        c
          | isAlpha c ->
              let (str, rest) = makeIdentifier trimmed
               in if str == "mod"
                    then Right . Just $ (Operator Modulus, rest)
                    else Left ("Illegal Token found: '" ++ str ++ "' is undefined")
        c
          | isNumber c ->
              let (num, rest) = makeNumber trimmed in Right . Just $ (Number num, rest)
        c -> Left ("Illegal Token found: '" ++ c : "' is undefined")
    where
      trimmed = dropWhile (`elem` " \t\n\r") src

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
makeNumber str = (\(x, y) -> (read x, y)) $ makeNumber' [] str False
  where
    makeNumber' :: String -> String -> Bool -> (String, String)
    makeNumber' acc src dots = case src of
      "" -> (acc, "")
      c : cs ->
        if c == '.' && dots
          then
            (acc, src)
          else
            if c `elem` "0123456789."
              then makeNumber' (acc ++ [c]) cs (c == '.')
              else (acc, src)
