module JSCompiler.Compiler where

import Parser.AST
import Data.List

compileExpression :: Expression -> String
compileExpression (EInt i) = show i
compileExpression (EBool b) = if b then "true" else "false"
compileExpression (EPlus l r) = compileExpression l ++ "+" ++ compileExpression r
compileExpression (EMinus l r) = compileExpression l ++ "-" ++ compileExpression r
compileExpression (ETimes l r) = compileExpression l ++ "*" ++ compileExpression r
compileExpression (ENested e) = "(" ++ compileExpression e ++ ")"
compileExpression (EAssign es) = let ds = fmap (\e -> (compileDeclarePatern $ fst e) ++ "= " ++ (compileExpression $ snd e)) es
                                 in concat $ intersperse "; " ds
compileExpression (ETuple es) = let exprs = fmap compileExpression es
                                in "[" ++ (concat (intersperse ", " exprs)) ++ "]"

compileDeclarePatern :: DeclarePatern -> String
compileDeclarePatern (DPSingleId i) = "const " ++ i
compileDeclarePatern (DPDescructTuple is) = "const [" ++ intercalate ", " is ++ "]"
compileDeclarePatern (DPDescructArray is) = "const [" ++ intercalate ", " is ++ "]"

compile :: AST -> String
compile ast = let exprs = fmap compileExpression ast in intercalate ";\n" exprs
