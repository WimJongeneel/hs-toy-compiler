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
tunit         { TUnit }
tfunc         { TFunc }
comma         { Comma }
lbr           { LeftBracket }
rbr           { RightBracket }
colon         { Colon }

%right eq comp compNot dot in arrow
%left plus minus
%left times divide
%right if then else divider

%%
 
AST : Expression AST                                                { $1:$2 }
    | {- empty -}                                                   { [ ] }

Expression: if Expression then Expression else Expression           { EIfElse $2 $4 $6 }
  | if Expression then Expression                                   { EIf $2 $4 }
  | Expression comp Expression                                      { ECompare $1 $3 }
  | Expression compNot Expression                                   { ECompareNot $1 $3 }
  | Expression plus Expression                                      { EPlus $1 $3 }
  | Expression minus Expression                                     { EMinus $1 $3 }
  | Expression times Expression                                     { ETimes $1 $3 }
  | Expression divide Expression                                    { EDivide $1 $3 }
  | let LetDeclarations in Expression                               { ELetIn $2 $4 }
  | let LetDeclarations                                             { EAssign $2 }
  | match Expression with Paterns                                   { EMatch $2 $4 }
  | Expression1                                                     { $1 }

Expression1 : Expression1 lp Expression rp                          { ECall $1 $3 }
  | id arrow Expression                                             { EFunction $1 $3 }
  | lsb ExpressionListDivider rsb                                   { EArrayInit $2 }
  | lsb rsb                                                         { EArrayInit [] }
  | Expression1 dot lsb Expression rsb                              { EIndex $1 $4 }
  | Expression1 dot id                                              { EObjectIndex $1 $3 }
  | lbr rbr                                                         { EObject [] }
  | lbr PropertyList rbr                                            { EObject $2 }
  | true                                                            { EBool True }
  | false                                                           { EBool False }
  | int                                                             { EInt $1 }
  | lp ExpressionListComma rp                                       { if length $2 == 1 then ENested ($2 !! 0) else ETuple $2 }
  | id                                                              { ERead $1 }

PropertyList: id colon Expression divider PropertyList              { ($1, $3):$5 }
  | id colon Expression                                             { [($1, $3)] }

ExpressionListDivider: Expression divider ExpressionListDivider     { $1:$3 }
  | Expression                                                      { [$1] }

ExpressionListComma: Expression comma ExpressionListComma           { $1:$3 }
  | Expression                                                      { [$1] }

LetDeclarations: LetDeclaration divider LetDeclarations             { $1:$3 }
  | LetDeclaration                                                  { [$1] }

LetDeclaration: id eq Expression                                    { (DPSingleId $1, $3) }
  | lp IdListComma rp eq Expression                                 { (DPDescructTuple $2, $5) }

IdListComma: id comma IdListComma                                   { $1:$3 }
  | id                                                              { [$1] }

Paterns: pipe Patern arrow Expression Paterns                       { ($2, $4):$5 }
  | pipe Patern arrow Expression                                    { [($2, $4)] }

Patern: tbool                                                       { PBoolType }
  | tint                                                            { PIntType }
  | int                                                             { PIntValue $1 }
  | true                                                            { PBoolValue True }
  | false                                                           { PBoolValue False }
  | underscore                                                      { PNone }
  | tunit                                                           { PUnitType }
  | tfunc                                                           { PFuncType }
  | lsb PaternList rsb                                              { PArray False $2 }
  | lsb PaternList dot dot rsb                                      { PArray True $2 }

PaternList: Patern divider PaternList                               { $1:$3 }
  | {- empty -}                                                     { [ ] }

{
happyError :: [Token] -> a
happyError i = error $ show i
}