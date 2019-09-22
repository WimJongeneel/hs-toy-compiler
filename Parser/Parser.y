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

%right eq
%left plus minus
%left times divide

%%
 
AST : Expression AST      { $1:$2 }
    | {- empty -}         { [ ] }

Expression: Expression plus Expression        { EPlus $1 $3 }
  | Expression minus Expression               { EMinus $1 $3 }
  | Expression times Expression               { ETimes $1 $3 }
  | Expression divide Expression              { EDivide $1 $3 }
  | lp Expression rp                          { ENested $2 }
  | let id eq Expression                      { EAssign "hi" $4}
  | int                                       { EInt $1 }
  | id                                        { ERead $1 }

{
happyError :: [Token] -> a
happyError i = error $ show i
}