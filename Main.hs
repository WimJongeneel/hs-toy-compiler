module Main where

import Lexer.Lexer
import Parser.Parser
import Runtime.RunExpression
import Parser.AST

astToString :: AST -> String
astToString ast = show $ fmap show ast

main ::IO ()
main = do
  -- let stdIn = "let x = [1; 2]; y = 1; z = [true;false;true] \
  --              \ z.[1 + 1]"
  -- let stdIn = "let x = 20; y = x + 10 in x + y"
  let stdIn = "let x = 0 \
              \ let ss = let x = 20; y = x + 10 in x + y \
              \ x"

  -- let stdIn = "let x = 1"
  let tokens = alexScanTokens stdIn
  print tokens
  let ast = parse tokens
  print $ astToString ast
  let r = execProgram ast
  
  print $ fst r
  print $ snd r