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