# Haskell Toy Compiler

Made in Haskell with Alex and Happy.

## Variables

```sh
let x = 10
let y = true
let a = [ 1; 2; 3 ]
```

## Conditionals

```hs
let x = if 1 == 1 then 1 else 2
```

## Locals

```hs
let x = let y = 10; z = y * 2 in y + z
```
