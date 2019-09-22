module Main where

import Lexer.Lexer
import Parser.Parser
import Runtime.RunExpression
import Parser.AST

astToString :: AST -> String
astToString ast = show $ fmap show ast

main ::IO ()
main = do
  let tokens = alexScanTokens "let x = 1 x + 1"
  print tokens
  let ast = parse tokens
  print $ astToString ast
  let r = execProgram ast
  
  print $ fst r
  print $ snd r