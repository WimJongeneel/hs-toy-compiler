{
module Parser.Parser(parse) where

import Lexer.Tokens
import Parser.AST
import qualified Lexer.Lexer as L
}

%name parse
%tokentype {Token}

%token
plus          { Plus }
minus         { Minus }
times         { Times }
divide        { Divide }
int           { Int $$ }
lp            { LeftParentheses }
rp            { RightParentheses }
let           { Let }
eq            { Equals }
id            { Id $$ }
then          { Then }
if            { If }
else          { Else }
comp          { Compare }
compNot       { CompareNot }
true          { TTrue }
false         { TFalse }
lsb           { TLSB }
rsb           { TRSB }
divider       { Divider }
dot           { Dot }
in            { In }
arrow         { Arrow }
match         { Match }
with          { With }
pipe          { Pipe }
tbool         { TBool }
tint          { TInt }
underscore    { Underscore }

%right eq comp compNot dot in arrow
%left plus minus
%left times divide
%right if then else divider

%%
 
AST : Expression AST                                        { $1:$2 }
    | {- empty -}                                           { [ ] }

Expression: if Expression then Expression else Expression   { EIfElse $2 $4 $6 }
  | if Expression then Expression                           { EIf $2 $4 }
  | Expression comp Expression                              { ECompare $1 $3 }
  | Expression compNot Expression                           { ECompareNot $1 $3 }
  | Expression plus Expression                              { EPlus $1 $3 }
  | Expression minus Expression                             { EMinus $1 $3 }
  | Expression times Expression                             { ETimes $1 $3 }
  | Expression divide Expression                            { EDivide $1 $3 }
  | let LetDeclarations in Expression                       { ELetIn $2 $4 }
  | let LetDeclarations                                     { EAssign $2 }
  | match Expression with Paterns                           { EMatch $2 $4 }
  | Expression1                                             { $1 }

Expression1 : Expression1 lp Expression rp                  { ECall $1 $3 }
  | id arrow Expression                                     { EFunction $1 $3 }
  | lsb ExpressionList rsb                                  { EArrayInit $2 }
  | Expression1 dot lsb Expression rsb                      { EIndex $1 $4 }
  | true                                                    { EBool True }
  | false                                                   { EBool False }
  | int                                                     { EInt $1 }
  | id                                                      { ERead $1 }
  | lp Expression rp                                        { ENested $2 }

ExpressionList: Expression divider ExpressionList           { $1:$3 }
  | Expression                                              { [$1] }

LetDeclarations: id eq Expression divider LetDeclarations   { ($1, $3):$5 }
  | id eq Expression                                        { [($1, $3)] }

Paterns: pipe Patern arrow Expression Paterns               { ($2, $4):$5 }
 | pipe Patern arrow Expression                             { [($2, $4)] }

Patern: tbool                                               { PBoolType }
  | tint                                                    { PIntType }
  | int                                                     { PIntValue $1 }
  | true                                                    { PBoolValue True }
  | false                                                   { PBoolValue False }
  | underscore                                              { PNone }
  | lsb PaternList rsb                                      { PArray False $2 }
  | lsb PaternList dot dot rsb                              { PArray True $2 }

PaternList: Patern divider PaternList                       { $1:$3 }
  | {- empty -}                                             { [] }

{
happyError :: [Token] -> a
happyError i = error $ show i
}