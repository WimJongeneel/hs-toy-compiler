module Lexer.Tokens where

data Token = Times
  | Minus
  | Plus
  | Int Int
  | Divide
  | LeftParentheses
  | RightParentheses
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
  | Match
  | With
  | Pipe
  | TBool
  | TInt
  | TFunc
  | TUnit
  | Underscore
  | Comma
  | LeftBracket
  | RightBracket
  | Colon
  deriving (Eq, Show)