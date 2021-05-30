Review of the off-chain (wallet) part of the code

In the off-chain part is pure Haskell (there is no Plutus). All the code is written in a special Monad fashion. 

Monads briefing:

In functions with other programming languages, there is no clue what happened between the 2 calls of the same function. This is because IO which can change the result. IO is any Input given to the function.
Haskell functions are not succeptible to those Inputs that may change the result of the function. 

But IO is desirable to have useful programs. Way to declare inputs/oututs to functions:

- In Haskell: IO Monad
  ```haskell
  foo :: IO Int
  foo = ...
  ```

It means that the function is computed but may contain side effects.

That brings us to:

## Maybe

- Definition
  ```haskell
  data Maybe a = Nothing | Just a
 
