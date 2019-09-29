module Parser.AST where

data Expression = EPlus Expression Expression
  | EMinus Expression Expression
  | ETimes Expression Expression
  | EDivide Expression Expression
  | EInt Int
  | ENested Expression
  | EAssign String Expression
  | ERead String
  | EIf Expression Expression
  | EIfElse Expression Expression Expression
  | ECompare Expression Expression
  | ECompareNot Expression Expression
  | EBool Bool
  | EArrayInit [Expression]
  deriving (Eq, Show)

type AST = [Expression]