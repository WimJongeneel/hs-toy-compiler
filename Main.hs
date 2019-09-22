module Main where

import Lexer.Lexer
import Parser.Parser
import Runtime.RunExpression
import Parser.AST

astToString :: AST -> String
astToString ast = show $ fmap show ast

main ::IO ()
main = do
  let tokens = alexScanTokens "1+1"
  print tokens
  let ast = parse tokens
  print $ astToString ast
  let r = fmap run ast
  print $ show r