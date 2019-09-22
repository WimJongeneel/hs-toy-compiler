module Runtime.RunExpression where

import Control.Monad.State
import Parser.AST
import qualified Data.Map as Map

data Value = VInt Int
  | VUnit
  deriving (Eq, Show)

type Memory = [Map.Map String Value]

emptyMemory :: Memory
emptyMemory = [Map.empty]

runExpression :: Expression -> State Memory Value
runExpression (EInt i) = state $ \m -> (VInt i, m)

runExpression (EPlus l r) = do
  valL <- runExpression l
  valR <- runExpression r
  let newVal = (case (valL, valR) of (VInt il, VInt ir) -> il + ir)
  return $ VInt newVal

runExpression (EMinus l r) = do
  valL <- runExpression l
  valR <- runExpression r
  let newVal =(case (valL, valR) of  (VInt il, VInt ir) -> il - ir)
  return $ VInt newVal

runExpression (ETimes l r) = do
  valL <- runExpression l
  valR <- runExpression r
  let newVal = (case (valL, valR) of (VInt il, VInt ir) -> il * ir)
  return $ VInt newVal

runExpression (EDivide l r) = do
  valL <- runExpression l
  valR <- runExpression r
  let newVal = (case (valL, valR) of (VInt il, VInt ir) -> il `div` ir)
  return $ VInt newVal

runExpression (ENested e) = runExpression e

runExpression (EAssign id' e) = do
  m <- get
  value <- runExpression e
  let scope = head m
  put [Map.insert id' value scope]
  return VUnit

runExpression (ERead id') = do
  m <- get 
  case Map.lookup id' $ head m of
    Just v    -> return v
    Nothing   -> return VUnit

runProgram :: AST -> State Memory Value
runProgram [] = state $ \m -> (VUnit, emptyMemory)
runProgram [e] = runExpression e
runProgram (e:ast) = state $ \m0 -> let 
                                      s = runExpression e
                                      (_, m1) = runState s m0
                                      s1 = runProgram ast
                                    in runState s1 m1

execProgram :: AST -> (Value, Memory)
execProgram ast = let p = runProgram ast in runState p emptyMemory
