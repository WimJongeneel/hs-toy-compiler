module Lexer.Tokens where

data Token = Times
  | Minus
  | Plus
  | Int Int
  | Divide
  | LeftParentheses
  | RigthParentheses
  | Let
  | Equals
  | Id String
  | If
  | Else
  | Then
  | Compare
  | CompareNot
  | TTrue 
  | TFalse
  | TLSB
  | TRSB
  | Divider
  | Dot
  | In
  | Arrow
  deriving (Eq, Show)