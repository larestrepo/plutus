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


All of them of type Data which means that it can take any data type. But of course, we need to be more specific and we should define data types for the 3 function inputs according to the needs. 
For example:
```haskell
    mkValidator :: () -> Integer -> ScriptValidator -> Bool
    mkValidator () r _
        | r == 42   = True
        | otherwise = False
        
 ```
 In this function we have: Datum :: unit; Redeemer :: Integer; Validator :: ValidatorCtx
 
 As the validtor is expecting Data as data type we need to make the "conversion" by using data Typed as follows:
 ```haskell 
     data Typed
    instance Scripts.ScriptType Typed where
        type instance DatumType Typed    = ()
            type instance RedeemerType Typed = Integer
```
The haskell function needs to be converted into a Plutus validator function to be compiled. 
 ```haskell
    typedValidator :: Scripts.TypedValidator Typed
    typedValidator = Scripts.mkTypedValidator @Typed
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @() @Integer

wrap allows to convert to datatype to not datatype
```
Combined with the inlinable 
 
     {-# INLINABLE mkValidator #-}
     
 In the definition of the validator, it is also possible to use traceIfFalse function from PlutusTx.prelude which is of type: String -> Bool -> Bool:
 
 ```haskell
 {-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
````

With PlutusTx.IsData.Class is possible to convert to a -> data and from data -> maybe a
     
 Use custom datatypes
 
 ```haskell
 newtype MySillyRedeemer = MySillyRedeemer Integer

PlutusTx.unstableMakeIsData ''MySillyRedeemer
```
  
     
