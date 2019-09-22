module Runtime.RunExpression where

import Parser.AST

run :: Expression -> Int
run (EInt i)      = i
run (EPlus l r)   = let il = run l; ir = run r in il + ir
run (EMinus l r)  = let il = run l; ir = run r in il - ir
run (ETimes l r)  = let il = run l; ir = run r in il * ir
run (EDivide l r) = let il = run l; ir = run r in il `div` ir
run (ENested e)   = run e
