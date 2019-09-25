{-# LANGUAGE TupleSections #-}

module Runtime.RunExpression where

import Control.Monad.State
import Parser.AST
import qualified Data.Map as Map

data Value = VInt Int
  | VUnit
  | VBool Bool
  deriving (Eq, Show)

type Memory = [Map.Map String Value]

emptyMemory :: Memory
emptyMemory = [Map.empty]

runNumericBinaryExpression :: (Expression -> State Memory Value)
  -> Expression
  -> (Int -> Int -> Int)
  -> Expression
  -> State Memory Value
runNumericBinaryExpression run left opp rigth = do
  v1 <- run left
  r1 <- run rigth
  let v = case (v1, r1) of 
           (VInt il, VInt ir)     -> il `opp` ir
           _                      -> error "invalid opperator"
  return $ VInt v

runBoolBinaryExpression :: (Expression -> State Memory Value)
  -> Expression
  -> (Value -> Value -> Bool)
  -> Expression
  -> State Memory Value
runBoolBinaryExpression run left opp rigth = do
  l1 <- run left
  r1 <- run rigth
  return $ VBool $ l1 `opp` r1

runExpression :: Expression -> State Memory Value
runExpression (EInt i)        = state (VInt i,)
runExpression (EPlus l r)     = runNumericBinaryExpression runExpression l (+) r
runExpression (EMinus l r)    = runNumericBinaryExpression runExpression l (-) r
runExpression (ETimes l r)    = runNumericBinaryExpression runExpression l (*) r
runExpression (EDivide l r)   = runNumericBinaryExpression runExpression l div r
runExpression (ENested e)     = runExpression e
runExpression (EAssign id' e) = do
  m <- get
  value <- runExpression e
  let scope = head m
  put [Map.insert id' value scope]
  return VUnit
runExpression (ERead id')     = do
  m <- get
  case Map.lookup id' $ head m of
    Just v    -> return v
    Nothing   -> return VUnit
runExpression (EIf c e)       = do
  c' <- runExpression c
  case c' of
    VBool b    -> if b then runExpression e else return VUnit
    _          -> return VUnit
runExpression (EIfElse c e1 e2) = do
  c' <- runExpression c
  case c' of
    VBool b   -> runExpression $ if b then e1 else e2
    _         -> runExpression e2
runExpression (ECompare l r) = runBoolBinaryExpression runExpression l (==) r
runExpression (ECompareNot l r) = runBoolBinaryExpression runExpression l (/=) r

runProgram :: AST -> State Memory Value
runProgram []      = state (VUnit,)
runProgram [e]     = runExpression e
runProgram (e:ast) = state $ \m0 -> let
                                      s = runExpression e
                                      (_, m1) = runState s m0
                                      s1 = runProgram ast
                                    in runState s1 m1

execProgram :: AST -> (Value, Memory)
execProgram ast = let p = runProgram ast in runState p emptyMemory
