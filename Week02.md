# These are my personal notes 

### Week2

DataTypes

newtype **BuiltinData**

    Important note about BuildinData: you should use this type in your on-chain code, and in any data structures that you want to be representable on-chain.

    For off-chain usage, there are conversion functions builtinDataToData and dataToBuiltinData, but note that these will not work on-chain.
    
 It is located in PlutusTx.
 
 BuiltinData does not have constructors as datatype, but there are ways to convert to Data where constructors are defined from which is possible to get values from BuiltinData, by using builtinDataToData and dataToBuiltinData.
 
 ```Haskell

Constr Integer [Data]	 
Map [(Data, Data)]	 
List [Data]	 
I Integer	 
B ByteString
```
 
## The give contract 

The validator is a function in haskell that takes 3 arguments and returns the unit type:

  Datum
  Redeemer
  Context
```haskell  
    mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    mkValidator _ _ _ = ()
  
```

To convert the validator into Plutus core, we need to compile it by using the following lines, we convert Haskell into Plutus Core language at compile time.

```Haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

The mkValidatorScript function takes the type CompiledCode (Data -> Data -> Data -> ()) -> Validator. In order to create this type, we must compile the mkValidator script using something called Template Haskell.

Template Haskell is an advanced feature of Haskell that solves a similar problem as macro systems in other languages. A macro being something that gets expanded at compile time.

So, with this code

    $$(PlutusTx.compile [|| mkValidator ||])
    
We are asking the compiler to write the code for the validator function at compile time based on our mkValidator function, and then proceed with the normal compilation.

With more complicated validators you will likely be relying on multiple helper functions, and you do not want to have to add them within the Oxford Brackets. To avoid this, there is one thing we need to do to the mkValidator function, and that is to make it inlinable by adding the INLINABLE pragma.

```Haskell
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()
```

The next thing is the Validator hash and address. We can obtain them like this:
```Haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

With the set -XOverloadedStrings command, it is possible to convert Strings into ByteStrings.

Now it comes the off-chain code

```haskell
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
```
We have 2 endpoints: give and grab

give builds the transaction to pay to the script with mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o

It returns TxConstraints i o, in the code is stored in variable tx, taking the ValidatorHash, arbitrary datum and the amount (value)

Then build and submit the transaction that satisfies the constraints using submitTx

For the grap endpoint:

look at the utxos sitting at the scriptAddress using **utxosAt** (Get the unspent transaction outputs at an address.)
lookups are the utxos and the validator information. Validator is needed because if you want to consume a UTxO sitting at a script address, then the spending transaction needs to provide the validator code, whereas the transaction that produces the UTxO only needs to provide the hash.

then we use the mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o

## The burn contract 

Validator that always fails.
```Haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = traceError "BURNT!"
```
In this case, error function is used (error function defined in Plutus and not in Haskell) located at PlutsuTx.Prelude

But standard error function in Haskell exists. So the script must know which one to use. This is done by adding line {-# LANGUAGE NoImplicitPrelude   #-}. It avoids GHC to import the standard Haskell prelude functions and instead use the PlutusTx.Prelude provided this is imported: 
```haskell
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
```

However Stardard prelude is still imported in 
```haskell
import           Prelude             (IO, Semigroup (..), String)
```

But after running the contract there is no log message that help to know the reasons why it failed. For that we can use traceError function in Pluts
```haskell
traceError :: BuiltinString -> a
```
It takes a BuiltinString and produce arbitrary data. To use BuiltinString we need to import to deal with ByteStrings that Plutus understands. 
```haskell
{-# LANGUAGE OverloadedStrings   #-}
```

## The Forty-two contract 

Now we want to use something as redeemer. The idea is that redeemer value is equal to 42 it succeedes otherwise it fails. 

```haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = traceError "wrong redeemer!"
```
the Builtins is just to convert BuiltinData into Data type.

Now the endpoints need to be changed a bit, because the grab now needs to provide some integer value to be validated in the script. 

The main changes in the code is that we pass now integer as an argument in the grab endpoint validation.

```haskell
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab n = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

```

So far we have worked with a high level data type representation. But we want to be more specific depending on the business case while defining the data types of Datum, Redeemer and Context. 

For example, from the previous contract we could be more specific like this:
```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

Emit the given BuiltinString only if the argument evaluates to False.
```haskell
traceIfFalse :: BuiltinString -> Bool -> Bool
```
It means that it prints the BuiltinString if the argument evaluates to false.

Now we need to define New data typed that encodes the information of the Datum and the redeemer to properly compile.

This is done in data Typed definition 
 
 ```haskell 
     data Typed
    instance Scripts.ScriptType Typed where
        type instance DatumType Typed    = ()
            type instance RedeemerType Typed = Integer
```

We instantiate the Typed data with Scripts.ScriptType. Now we are using 
 ```haskell 
import qualified Ledger.Typed.Scripts as Scripts
 ```
instead of just LedgerScripts:
 ```haskell 
import qualified Ledger.Typed.Scripts as Scripts
 ```

The haskell function needs to be converted into a Plutus validator function to be compiled. 
 ```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

```
This is similar to the mkValidator code, but this type we also compile a wrapValidator function that takes the datum and redeemer types.

Up to this point the validator function will compile. Now we need to create the validator

```haskell
validator :: Validator
validator = Scripts.validatorScript typedValidator
```

Changes in the off-chain part:

In the give function we had mustPayToOtherScript; now we have mustPayToTheScript which is the typed version. This is a convenience script which allows us to pass in just () as we longer need to construct a value of type Data. We also no longer need to pass in the script hash.

But we could still use the mustPayToOtherScript


With PlutusTx.IsData.Class is possible to use toData to converta a value to Data. Or use fromData to try to convert a value from Data.

When examining IsDat class we see that there are few possible combinations for conversions like I or (). But we know that we are going to need more data types for our datum and redeemer. In order to do this, we would normally need to define an IsData instance for any type that we wish to use. This will allow us to tell the compiler how to do the back and forth conversions. However, this again would be tedious as it is such a mechanical process. So, there is a mechanism in Plutus that does this for us.

## The Forty-two contract 
With this in mind, let's use custom datatypes:

```Haskell
newtype MySillyRedeemer = MySillyRedeemer Integer

PlutusTx.unstableMakeIsData ''MySillyRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42
```

mySilly Redeemer takes integer and converts it into custom datatype.

What this line does PlutusTx.unstableMakeIsData ''MySillyRedeemer is that at compile time, the compiler will use the Template Haskell to write an IsData instance for us. So we don't need to do it by hand. 

  
     
