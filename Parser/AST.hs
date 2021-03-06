module Parser.AST where

data Expression = EPlus Expression Expression
  | EMinus Expression Expression
  | ETimes Expression Expression
  | EDivide Expression Expression
  | EInt Int
  | ENested Expression
  | ETuple [Expression]
  | EAssign [(DeclarePatern, Expression)]
  | ERead String
  | EIf Expression Expression
  | EIfElse Expression Expression Expression
  | ECompare Expression Expression
  | ECompareNot Expression Expression
  | EBool Bool
  | EArrayInit [Expression]
  | EIndex Expression Expression
  | ELetIn [(DeclarePatern, Expression)] Expression
  | EGCCollect
  | EFunction String Expression
  | ECall Expression Expression
  | EMatch Expression [(Patern, Expression)]
  | EObject [(String, Expression)]
  | EObjectIndex Expression String
  deriving (Eq, Show)

data DeclarePatern = DPSingleId String
  | DPDescructTuple [String]
  | DPDescructArray [String]
  deriving (Eq, Show)

data Patern = PIntValue Int
  | PBoolValue Bool
  | PIntType
  | PBoolType
  | PFuncType
  | PUnitType
  | PArray Bool [Patern]
  | PNone
  deriving (Eq, Show)

type AST = [Expression]