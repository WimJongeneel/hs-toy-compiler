module Parser.AST where

data Expression = EPlus Expression Expression
  | EMinus Expression Expression
  | ETimes Expression Expression
  | EDivide Expression Expression
  | EInt Int
  | ENested Expression
  | EAssign String Expression
  | ERead String
  deriving (Eq, Show)

type AST = [Expression]