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
  -- let stdIn = "let ss = let x = 20; a = [1;2] in let x = x * 10 in x \
  --             \ let aa = [1;2;3]"

  -- let stdIn = "let ss = let x = 20; a = [1;2; [3;4]] in let x = x * 10 in x \
  --             \ let aa = [1;2;3;[0;0]]"

  -- let stdIn = "let x = 1"

  let stdIn = "match 1 + 1 with \
              \| 1     -> 999 \
              \| 2     -> 666 \
              \| []    -> 777 \
              \| [1; 2;..] -> 111111 \
              \| [..]  -> 444 \
              \| _     -> 000"

  let tokens = alexScanTokens stdIn
  print tokens
  let ast = parse tokens
  print $ astToString ast
  let r = execProgram ast
  
  print $ fst r
  print $ snd r
  print $ markUnreachableHeapEntries $ snd r
