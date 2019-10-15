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

## Functions

Functions are defined as lambdas. They are lexial scoped and capture references variables in their closure.
Functions are first-class-citizens.

```hs
let f = x -> 1 + 2
let r = f(1)
```

## GC

This language uses a `mark-and-sweep` based garbage collector. The GC will build a Set of heap adreses that are still reachable from the stack (both direct and indirect). All the heap entries that are not in this Set will be removed. GC calls are blocking.
