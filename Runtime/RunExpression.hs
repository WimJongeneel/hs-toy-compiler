{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Runtime.RunExpression where

import Control.Monad.State
import Parser.AST
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import Data.Maybe

data Value = VInt Int
  | VUnit
  | VBool Bool
  | VPointer Int
  deriving (Eq, Show)

data HeapValue = HArray [Value]
  -- | HObject [Map.Map String Value]
  | HFunction { closure :: Map.Map String Value, param :: String, body :: Expression }
  deriving (Eq, Show)

data Memory = Memory { stack :: [Map.Map String Value]
  , heap :: Map.Map Int HeapValue
} deriving Show

emptyMemory :: Memory
emptyMemory = Memory [Map.empty] Map.empty

readVal :: String -> [Map.Map String Value] -> Value
readVal id' (scope : mem) = fromMaybe (readVal id' mem) (Map.lookup id' scope)
readVal _ []              = VUnit

insertVal :: String -> Value -> [Map.Map String Value] -> [Map.Map String Value]
insertVal id' val (scope:mem) = let scope' = Map.insert id' val scope in scope' : mem
insertVal _ _ []              = error "no scopes in mem"

nextHeapAdress :: Memory -> Int
nextHeapAdress mem = let ks = Map.keys $ heap mem
                     in case ks of [] -> 0 
                                   _  -> (maximum $ Map.keys (heap mem)) + 1

after :: [State a b]  -> [b] -> State a [b]
after [s1] res      = state $ \s -> let (b, s') = runState s1 s in (res ++ [b], s')
after (sm:coll) res = state $ \s -> let (b, s') = runState sm s in runState (after coll $ res ++ [b]) s'
after _ _           = state ([],)

collectPointers :: [Value] -> Set.Set Int
collectPointers vals = Set.fromList $ concatMap (\case VPointer p -> [p] 
                                                       _          -> [ ]) vals

collectStackPointers :: Memory -> Set.Set Int
collectStackPointers mem = let st = concatMap (\s -> let l = Map.toList s in fmap snd l) (stack mem)
                           in collectPointers st

expandPointers :: Memory -> Set.Set Int -> Set.Set Int
expandPointers mem ps = let hp = heap mem
                            psL = Set.toList ps
                            ps' = concatMap (\p -> case Map.lookup p hp of 
                                                   Just (HArray a)    -> p : let _ps = collectPointers a in Set.toList $ expandPointers mem _ps
                                                   _                  -> [p]) psL
                        in Set.fromList ps'

markUnreachableHeapEntries :: Memory -> Set.Set Int
markUnreachableHeapEntries mem = let stackPointers = collectStackPointers mem
                                     reachablePointers = expandPointers mem stackPointers
                                     heapAdresses = Map.keys (heap mem) 
                                 in Set.fromList $ heapAdresses \\ (Set.toList reachablePointers)

gcCollect:: Memory -> Memory
gcCollect mem = let 
                unreachable = markUnreachableHeapEntries mem
                h = heap mem
                s = stack mem
                in 
                Memory s $ Map.filterWithKey (\k _ -> Set.member k unreachable) h

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
           _                      -> error $ "invalid opperator (" ++ show v1 ++ ") (" ++ show r1 ++ ")"
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

doInScope:: (Expression -> State Memory Value)
  -> Memory
  -> [(String, Expression)]
  -> Expression
  -> (Memory, Value)
doInScope run initalM scope expr = 
  let inserts = fmap (\e -> do 
                            v <- runExpression $ snd e
                            m <- get
                            put $ Memory (insertVal (fst e) v $ stack m) (heap m)
                            return VUnit) scope;
      statements = inserts ++ [run expr];
      sm = after statements [];
      (vs, m) = runState sm initalM;
  in
      (Memory (stack initalM) (heap m), last vs)

runExpression :: Expression -> State Memory Value
runExpression (EInt i)        = state (VInt i,)
runExpression (EBool b)       = state (VBool b,)
runExpression (EPlus l r)     = runNumericBinaryExpression runExpression l (+) r
runExpression (EMinus l r)    = runNumericBinaryExpression runExpression l (-) r
runExpression (ETimes l r)    = runNumericBinaryExpression runExpression l (*) r
runExpression (EDivide l r)   = runNumericBinaryExpression runExpression l div r
runExpression (ENested e) = runExpression e
runExpression (EAssign exprs) = do
  let inserts = fmap (\e -> do 
                            v <- runExpression $ snd e
                            m <- get
                            put $ Memory (insertVal (fst e) v $ stack m) (heap m)
                            return VUnit) exprs
  _ <- after inserts []
  return VUnit
runExpression (ERead id') = gets (readVal id' . stack)
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
runExpression (EArrayInit a)    = do
  vals <- after (fmap runExpression a) []
  m <- get
  let nk = nextHeapAdress m
  let heap' = Map.insert nk (HArray vals) (heap m)
  let m' = Memory (stack m) heap'
  put m'
  return $ VPointer nk
runExpression (EIndex e i)      = do
  val <- runExpression e
  index <- runExpression i
  m <- get
  case (val, index) of
    (VPointer p, VInt i') -> let cv = Map.lookup p $ heap m 
                             in case cv of 
                                Just (HArray a) -> return $ a !! i'
                                _               -> error "invalid index expression"  
    _                     -> error "invalid index expression"
runExpression (ELetIn defs e)   = do
  m <- get
  let (m', v) = doInScope runExpression m defs e
  put m'
  return v
runExpression EGCCollect        = do
  mem <- get 
  put $ gcCollect mem
  return VUnit
runExpression (EFunction pn b)  = do
  mem <- get
  let nk = nextHeapAdress mem
  let f = HFunction Map.empty pn b
  let heap' = Map.insert nk f $ heap mem
  put $ Memory (stack mem) heap'
  return $ VPointer nk


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