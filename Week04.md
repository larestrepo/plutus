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

>> (>>) (sequence): It is a function that combines functions to be executed in sequence, but it ignores the results or does not use the result for the next function.
>> (>>=) (bind): Similar to previous but it does not ignore the result from the previous rather it uses it for the next function. 

There is another, very important, way to create IO actions, and that is to create recipes that immediately return results without performing any side effects.

That brings us to:

## Maybe

- Definition
  ```haskell
  data Maybe a = Nothing | Just a
 
 # Combining different monads
 
 example: 
  - E.g. `Contract w s e a` monad which runs in a wallet
  - `EmulatorTrace a` monad for testing


## Emulator trace monad

- `runEmulatorTrace :: `
  - Executes trace on emulated block chain
  - Requires the trace to execute
  - Expects `EmulatorConfig` which allows to define initial chain state
    - I.e. `Value` in wallets at start
    - `Value` is not only ADA but can be native tokens
    - There is a `defaultDist`, which every wallet 100 ADA
  - Returns
    - List of emulator events
    - `Maybe EmulatorError`
    - State of emulator


- Example of vesting trace
  ```haskell
  test :: IO ()
  test = runEmulatorTraceIO myTrace

  myTrace :: EmulatorTrace ()
  myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParmas
      { gpBeneficiary = pubKeyHash  . walletPubKey . Wallet $ 2
      , gpDeadline    = Slot 20
      , gpAmount      = 1000
      }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached slot " ++ show s
  ```
  - Allows
    - Calling endpoints with params
    - Waiting for slots
    - Logging

## `Contract` Monad

  contract w s e a

- Off-chain code which runs in wallet
- Type parameters
  - `a` overall result of computation
  - `w` logging, like in writer monad. 
    - But not only, it can be used to pass information to a subsequent contract.
  - `s` (schema) blockchain specific capabilities
    - Like waiting slots
    - Waiting for transaction
    - Finding out own private key
    - Handling specific endpoints
  - `e` blockchain specific capabilities
    - Types of error messages

- Simple Example
  ```haskell
  myContract1 :: Contract () BlockchainActions Text ()
  myContract1 = Contract.logInfo @Text "Hello from the contract"

  myTrace1 :: EmulatorTrace ()
  myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

  test1 :: IO ()
  test1 = runEmulatorTraceIO myTrace1
  ```
  - Note: Need to add the `@Text` since language extension `OverloadedStrings` is used
    - which allows that `""` are not only `String` but maybe of other types, like `Text`
    - `@Text` is a type application (requires language extension `TypeApplications`) and so compiler knows what type this `"Hello from the contract"` has


- Looking at `w`, logging
  - `w` must be a `Monoid`
    - `Monoid a` has functions
      - `mempty :: a` for an empty value and
      - `mappend :: a -> a -> a` for appending values together function

  - Example with `w` being `[Int]`
    ```haskell
    myContract4 :: Contract [Int] BlockchainActions Text ()
    myContract4 = do
      void $ Contract.waitNSlots 10
      tell [1]
      void $ Contract.waitNSlots 10
      tell [2]
      void $ Contract.waitNSlots 10

    myTrace4 :: EmulatorTrace ()
    myTrace4 = do
      h <- activateContractWallet (Wallet 1) myContract4

      void $ Emulator.waitNSlots 5
      xs <- observableState h
      Extras.logInfo $ show xs

      void $ Emulator.waitNSlots 10
      ys <- observableState h
      Extras.logInfo $ show ys

      void $ Emulator.waitNSlots 10
      zs <- observableState h
      Extras.logInfo $ show zs
    ```
  - Helps to communicate from `Contract`
    - To `Emulator`, but also to
    - PAB (Plutus application backend), covered later

- Bi-directional communication
  - Into `Contract` via endpoints
  - Out of `Contract` via `tell` mechanism

# Testing a contract in the repl:

Activate the nix-shell inside the plutus repo directory: __@__:~/plutus$ nix-shell
Move to plutus-pioneer-program/code/week04/
Access the repl: cabal repl
Load the Contract.hs module :l src/Week04/Contract.hs
And executing the test: myTest

## We can catch errors and handle them by creating a second contract to handle errors. 

    handleError :: forall w s e e' a.
      (e -> Contract w s e' a)  -- first argument type
      -> Contract w s e a       -- second argument type
      -> Contract w s e' a      -- return type

```haskell
myContract2 :: Contract () BlockchainActions Void ()
myContract2 = Contract.handleError
  (\err -> Contract.logError $ "Caught error: " ++ unpack err)
  myContract1
  ```
  
We have chosen the Void error type of this second contract. As this data type has no inhabitant in Haskell, this means that this contract can not have any errors. We do this in order to show that the error from the first contract is indeed handled. As we can see, the function that handles the error just takes this error and we unpack it to convert it to the String type (at this moment it is of type Text, as we declared it with the parameter e on myContract1). Then we append it to the string message and log it as an error to the console with Contract.logError

## Adding endpoints to the set of blockchain actions

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- ^ Add extensions on the top of the module file

type MySchema = BlockchainActions .\/ Endpoint "foo" Int
```
In the last line we have defined the type of the set of actions and we have called it MySchema. Then we used the operator .\/, which acts on types, not on values, to "add" the endpoints that we want, in this case the foo endpoint. The first argument to Endpoint is a type level string which represents the name of the endpoint, and the second argument is the parameter type (which type of value this endpoint takes).

In the last line we have defined the type of the set of actions and we have called it MySchema. Then we used the operator .\/, which acts on types, not on values, to "add" the endpoints that we want, in this case the foo endpoint. The first argument to Endpoint is a type level string which represents the name of the endpoint, and the second argument is the parameter type (which type of value this endpoint takes).

Once we have defined the endpoint, we can take the action defined by it using the trace emulator. First, we define our contract:

```haskell
myContract :: Contract () MySchema Text ()
myContract = do
    n <- endpoint @"foo"
    Contract.logInfo n
 ```
 
This contract just waits for some wallet to call the "foo" endpoint with some Int value and then logs it to the console. Then, we define the trace of the simulation, where now we can use the endpoint we have just defined:

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract
    callEndpoint @"foo" h 42
```

Note that now we are interested in calling the endpoint, for which it is necessary to use the handler. We do not use the Void $ activateCon... anymore, but we reference the function output with some variable, h (for handler) in this case.

## the write parameter: w

This type parameter can not be of any type but an instance of the type class Monoid. An example of data type which is an instance of this class is List. This parameter of the Contract monad is essential because it allows us to bring information back from the contract to the trace and also to the PAB, the Plutus Application Backend. We will be able to pass info back from the contract running in the wallet to the outside world. Let us see an example:

```haskell
myContract :: Contract [Int] BlockchainActions Text ()
myContract = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10 
```

In the execution of this contract we first wait for 10 Slots, then we pass info back (which has to be of type [Int], as we chose on the contract type declaration) using the tell statement, then we again wait for 10 Slots, and so on.

Now we define the trace as follows:

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```

With this trace, we are just observing the state communicated by the contract after a number of Slots. In particular, we wait 5 Slots and observe the state using the observableState function, to which we pass the handler h of the contract associated with the wallet. Because the first communication made by the contract happens after Slot 10, we will get an empty* list on the console. Then we wait for another 10 Slots and ask again for the state. Now the contract has already communicated something, as we have passed Slot 15 and the communications happened on Slot 10. In particular, we will observe the list [1] on the console. I'll let you guess what happens when we take a look at the contract state for the third time.
