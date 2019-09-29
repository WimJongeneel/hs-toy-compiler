{
module Lexer.Lexer
  (alexScanTokens)
where

import Lexer.Tokens
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                             ;
  ";"                                 { (\s -> Divider) }
  "["                                 { (\s -> TLSB )}
  "]"                                 { (\s -> TRSB )}
  "true"                              { (\s -> TTrue) }
  "false"                             { (\s -> TFalse) }
  "if"                                { (\s -> If) }
  "then"                              { (\s -> Then) }
  "else"                              { (\s -> Else) }
  "=="                                { (\s -> Compare) }
  "<>"                                { (\s -> CompareNot ) }
  "*"                                 { (\s -> Times) }
  "+"                                 { (\s -> Plus) }
  "-"                                 { (\s -> Minus) }
  "/"                                 { (\s -> Divide) }
  "("                                 { (\s -> LeftParentheses)}
  ")"                                 { (\s -> LeftParentheses)}
  "let"                               { (\s -> Let )}
  "="                                 { (\s -> Equals )}
  $digit+                             { (\s -> Int $ read s) }
  $alpha                              { (\s -> Id $ show s) }