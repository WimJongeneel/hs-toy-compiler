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
  "->"                                { (\_ -> Arrow) }
  "in"                                { (\_ -> In) }
  "."                                 { (\_ -> Dot) }
  ";"                                 { (\_ -> Divider) }
  "["                                 { (\_ -> TLSB )}
  "]"                                 { (\_ -> TRSB )}
  "true"                              { (\_ -> TTrue) }
  "false"                             { (\_ -> TFalse) }
  "if"                                { (\_ -> If) }
  "then"                              { (\_ -> Then) }
  "else"                              { (\_ -> Else) }
  "=="                                { (\_ -> Compare) }
  "<>"                                { (\_ -> CompareNot ) }
  "*"                                 { (\_ -> Times) }
  "+"                                 { (\_ -> Plus) }
  "-"                                 { (\_ -> Minus) }
  "/"                                 { (\_ -> Divide) }
  "("                                 { (\_ -> LeftParentheses) }
  ")"                                 { (\_ -> RightParentheses) }
  "let"                               { (\_ -> Let )}
  "="                                 { (\_ -> Equals )}
  $digit+                             { (\s -> Int $ read s) }
  $alpha+                             { (\s -> Id $ show s) }