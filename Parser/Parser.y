{
module Parser.Parser(parse) where

import Lexer.Tokens
import Parser.AST
import qualified Lexer.Lexer as L
}

%name parse
%tokentype {Token}

%token
plus      { Plus }
minus     { Minus }
times     { Times }
divide    { Divide }
int       { Int $$ }
lp        { LeftParentheses }
rp        { LeftParentheses }
let       { Let }
eq        { Equals }
id        { Id $$ }
then      { Then }
if        { If }
else      { Else }
comp      { Compare }
compNot   { CompareNot }
true      { TTrue }
false     { TFalse }

%right eq comp compNot
%left plus minus
%left times divide
%right if then else

%%
 
AST : Expression AST      { $1:$2 }
    | {- empty -}         { [ ] }

Expression: if Expression then Expression else Expression   { EIfElse $2 $4 $6 }
  | if Expression then Expression                           { EIf $2 $4 }
  | Expression comp Expression                              { ECompare $1 $3 }
  | Expression compNot Expression                           { ECompareNot $1 $3 }
  | Expression plus Expression                              { EPlus $1 $3 }
  | Expression minus Expression                             { EMinus $1 $3 }
  | Expression times Expression                             { ETimes $1 $3 }
  | Expression divide Expression                            { EDivide $1 $3 }
  | lp Expression rp                                        { ENested $2 }
  | let id eq Expression                                    { EAssign $2 $4}
  | true                                                    { EBool True }
  | false                                                   { EBool False }
  | int                                                     { EInt $1 }
  | id                                                      { ERead $1 }

{
happyError :: [Token] -> a
happyError i = error $ show i
}