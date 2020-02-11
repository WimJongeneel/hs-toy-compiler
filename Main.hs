module Main where

import Lexer.Lexer
import Parser.Parser
import Runtime.RunExpression
import Parser.AST
import JSCompiler.Compiler (compile)

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

  -- let stdIn = "match [true; false] with \
  --             \| 1     -> 999 \
  --             \| 2     -> 666 \
  --             \| []    -> 777 \
  --             \| [1; 2; ] -> 111111 \
  --             \| [1; 2; .. ] -> 222222 \
  --             \| [..]  -> 444 \
  --             \| _     -> 000"

  -- let stdIn = "let tt = (1, 2, 3, 4, 5)\
  --            \ let (q, w, e) = tt\
  --            \ let f = x -> let x = x * 2; xx = x * 3 in (x, xx)\
  --            \ let (o, p) = f(1)"

  -- let stdIn = "let x = { x: 1; w: 2 }\ 
  --             \let t ={}\
  --             \let xx = [1;2]\
  --             \xx.[1]"

  let stdIn = "let x = 1 + 1 \
              \let (z, y) = (true, false)"
  
  let tokens = alexScanTokens stdIn
  print tokens
  let ast = parse tokens
  print $ astToString ast
  print $ compile ast
  -- let r = execProgram ast
  
  -- print $ fst r
  -- print $ snd r
  -- print $ markUnreachableHeapEntries $ snd r
