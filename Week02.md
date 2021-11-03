# These are my personal notes 

### Week2

The validator is a function in haskell that takes 3 arguments and returns the unit type:

  Datum
  Redeemer
  Context
```haskell  
    mkValidator :: Data -> Data -> Data -> ()
    mkValidator _ _ _ = ()
  
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
  
     
