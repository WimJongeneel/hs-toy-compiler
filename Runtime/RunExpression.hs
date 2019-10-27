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
  | VTuple [Value]
  deriving (Eq, Show)

data HeapValue = HArray [Value]
  | HObject (Map.Map String Value)
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

insertVals :: [(String, Value)] -> [Map.Map String Value] -> [Map.Map String Value]
insertVals vals (scope : mem) =
  let scope' = Map.union scope (Map.fromList vals) in scope' : mem
insertVals _ [] = error "no scopes in mem"

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
                                                       VTuple vs  -> let set = collectPointers vs in Set.toList set
                                                       _          -> [ ]) vals

collectStackPointers :: Memory -> Set.Set Int
collectStackPointers mem = let st = concatMap (\s -> let l = Map.toList s in fmap snd l) (stack mem)
                           in collectPointers st

expandPointers :: Memory -> Set.Set Int -> Set.Set Int
expandPointers mem ps = let hp = heap mem
                            psL = Set.toList ps
                            ps' = concatMap (\p -> case Map.lookup p hp of 
                                                   Just (HArray a)    -> p : let _ps = collectPointers a in Set.toList $ expandPointers mem _ps
                                                   Just (HObject a)   -> p : let _ps = collectPointers (fmap snd (Map.toList a)) in Set.toList $ expandPointers mem _ps
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

runNumericBinaryExpression :: Expression
  -> (Int -> Int -> Int)
  -> Expression
  -> State Memory Value
runNumericBinaryExpression left opp rigth = do
  v1 <- runExpression left
  r1 <- runExpression rigth
  let v = case (v1, r1) of 
           (VInt il, VInt ir)     -> il `opp` ir
           _                      -> error $ "invalid opperator (" ++ show v1 ++ ") (" ++ show r1 ++ ")"
  return $ VInt v

runBoolBinaryExpression :: Expression
  -> (Value -> Value -> Bool)
  -> Expression
  -> State Memory Value
runBoolBinaryExpression left opp rigth = do
  l1 <- runExpression left
  r1 <- runExpression rigth
  return $ VBool $ l1 `opp` r1

doInScope:: Memory
  -> [(DeclarePatern, Expression)]
  -> Expression
  -> (Memory, Value)
doInScope initalM scope expr = 
  let inserts = fmap (\e -> do 
                            v <- runExpression $ snd e
                            m <- get
                            let newEnties =
                                 case fst e of
                                 DPSingleId i      -> [(i, v)]
                                 DPDescructTuple i -> case v of 
                                                      VTuple vs -> zip i vs
                                                      _         -> []
                                 _                 -> []
                            put $ Memory (insertVals newEnties (stack m)) (heap m)
                            return VUnit) scope;
      statements = inserts ++ [runExpression expr]
      sm = after statements [];
      (vs, m) = runState sm initalM;
  in
      (Memory (stack initalM) (heap m), last vs)

getReferencedIdentifiers :: Expression -> [String]
getReferencedIdentifiers (ERead i)          = [i]
getReferencedIdentifiers (ENested e)        = getReferencedIdentifiers e
getReferencedIdentifiers (ECompare l r)     = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (EPlus l r)        = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (EMinus l r)       = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (EDivide l r)      = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (ETimes l r)       = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (ECompareNot l r)  = getReferencedIdentifiers l ++ getReferencedIdentifiers r
getReferencedIdentifiers (EArrayInit es)    = concatMap getReferencedIdentifiers es
getReferencedIdentifiers (EIndex e i)       = getReferencedIdentifiers e ++ getReferencedIdentifiers i
getReferencedIdentifiers (ELetIn d e)       = let 
                                              d' = concatMap (getReferencedIdentifiers . snd) d
                                              e' = getReferencedIdentifiers e
                                              in d' ++ e'
getReferencedIdentifiers (EFunction _ b)    = getReferencedIdentifiers b
getReferencedIdentifiers (ECall f p)        = getReferencedIdentifiers f ++ getReferencedIdentifiers p
getReferencedIdentifiers (ETuple vals)      = concatMap getReferencedIdentifiers vals
getReferencedIdentifiers _                  = []

runExpression :: Expression -> State Memory Value
runExpression (EInt i)        = state (VInt i,)
runExpression (EBool b)       = state (VBool b,)
runExpression (ETuple exprs)  = do
  vals <- after (fmap runExpression exprs) []
  return $ VTuple vals
runExpression (EPlus l r)     = runNumericBinaryExpression l (+) r
runExpression (EMinus l r)    = runNumericBinaryExpression l (-) r
runExpression (ETimes l r)    = runNumericBinaryExpression l (*) r
runExpression (EDivide l r)   = runNumericBinaryExpression l div r
runExpression (ENested e)     = runExpression e
runExpression (EAssign exprs) = do
  let inserts = fmap (\e -> do 
                            v <- runExpression $ snd e
                            m <- get
                            let newEnties =
                                 case fst e of
                                 DPSingleId i      -> [(i, v)]
                                 DPDescructTuple i -> case v of 
                                                      VTuple vs -> zip i vs
                                                      _         -> []
                                 _                 -> []
                            put $ Memory (insertVals newEnties (stack m)) (heap m)
                            return VUnit) exprs
  _ <- after inserts []
  return VUnit
runExpression (ERead id')     = gets (readVal id' . stack)
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
runExpression (ECompare l r) = runBoolBinaryExpression l (==) r
runExpression (ECompareNot l r) = runBoolBinaryExpression l (/=) r
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
    -- TODO: recursive lookup incase a pointer points to a pointer (can this happen?)
    (VPointer p, VInt i') -> let cv = Map.lookup p $ heap m 
                             in case cv of 
                                Just (HArray a) -> return $ a !! i'
                                _               -> error "invalid index expression"

    _                     -> error "invalid index expression"
runExpression (ELetIn defs e)   = do
  m <- get
  let (m', v) = doInScope m defs e
  put m'
  return v
runExpression EGCCollect        = do
  mem <- get 
  put $ gcCollect mem
  return VUnit
runExpression (EFunction pn b)  = do
  mem <- get
  let nk = nextHeapAdress mem
  let c' = getReferencedIdentifiers b
  let c'' = fmap (\i -> (i, readVal i (stack mem))) c'
  let f = HFunction (Map.fromList c'') pn b
  let heap' = Map.insert nk f $ heap mem
  put $ Memory (stack mem) heap'
  return $ VPointer nk
runExpression (ECall f a)       = do
  fun <- runExpression f
  arg <- runExpression a
  mem <- get 
  let (scope, pId, body) =  case fun of
                              VPointer p -> case Map.lookup p $ heap mem of 
                                             Just (HFunction s p b) -> (s, p, b)
                                             _                      ->  error "invalid function call"
                              _          -> error "invalid function call"
  let scope' = Map.insert pId arg scope
  let (v, mem') = let s = runExpression body in runState s (Memory [scope'] (heap mem))
  -- we want the updated heap from the function call (in case the function returned a complex type we need that heap entry)
  -- but we don't whant the internal stack of the function
  put $ Memory (stack mem) (heap mem')
  return v
runExpression (EMatch val ps)   = do
  value <- runExpression val
  mem <- get
  let pattern = find (\pe -> patternMatches mem (fst pe) value) ps
  case pattern of
    Just (_, e)   -> runExpression e
    _             -> return VUnit
runExpression (EObject props)   = do
  vals <- after (fmap (runExpression . snd) props) []
  let propnames = fmap fst props
  let props' = Map.fromList $ zip propnames vals
  m <- get
  let nk = nextHeapAdress m
  let heap' = Map.insert nk (HObject props') (heap m)
  let m' = Memory (stack m) heap'
  put m'
  return $ VPointer nk
runExpression (EObjectIndex e i) = do
  val <- runExpression e
  m <- get
  case val of
    -- TODO: recursive lookup incase a pointer points to a pointer (can this happen?)
    VPointer p -> let cv = Map.lookup p $ heap m 
                  in case cv of 
                     Just (HObject p) -> fromMaybe (return VUnit) (fmap return (Map.lookup i p))
                     _                -> error "invalid index expression"

    _                     -> error "invalid index expression"
reducePointerToValue :: Memory -> Int -> Maybe HeapValue
reducePointerToValue mem p = let v = Map.lookup p $ heap mem in case v of 
                                                                Just (HArray _) -> v
                                                                _               -> Nothing

patternMatches :: Memory -> Patern -> Value -> Bool
patternMatches _ (PIntValue i1) (VInt i2)     = i1 == i2
patternMatches _ (PIntValue _) _              = False
patternMatches _ (PBoolValue b1) (VBool b2)   = b1 == b2
patternMatches _ (PBoolValue _) _             = False
patternMatches _ PIntType (VInt _)            = True
patternMatches _ PBoolType (VBool _)          = True
patternMatches _ PNone _                      = True
patternMatches m pt (VPointer po)             = let val = reducePointerToValue m po 
                                                in case (pt, val) of
                                                    ((PArray _ []), (Just (HArray [])))   -> True
                                                    ((PArray True []), (Just (HArray _))) -> True
                                                    ((PArray _ []), (Just (HArray _)))    -> False
                                                    ((PArray o pts), (Just (HArray vs)))  -> let pv = zip pts vs; lm = o || length pts == length vs
                                                                                             in lm && all (\v -> patternMatches m (fst v) (snd v)) pv
                                                    _                                     -> False
patternMatches _ _ _                          = False

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