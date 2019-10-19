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

## Paternmatching

Paterns can be described using values or types. For array a collection of paterns can be used which will be used at their respoective index. With `..` we can indicate that their could be more items in the array. `_` is used to match everything.

```hs
match 1 with
| 2   -> 1
| int -> 2
| _   -> 0

match [1;2] with
| []      -> 1 --empty array
| [1]     -> 2 --array with 1 item: 1
| [1; ..] -> 3 --array with n items, of which the first is 1
```

## GC

This language uses a `mark-and-sweep` based garbage collector. The GC will build a Set of heap adreses that are still reachable from the stack (both direct and indirect). All the heap entries that are not in this Set will be removed. GC calls are blocking.
