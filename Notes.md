# These are my personal notes 

### Week2

The validator is a function in haskell that takes 3 arguments and returns the unit type:

  Datum
  Redeemer
  Context
  
    mkValidator :: Data -> Data -> Data -> ()
    mkValidator _ _ _ = ()
  
All of them of type Data which means that it can take any data type. But of course, we need to be more specific and we should define data types for the 3 function inputs according to the needs. 
For example:

    mkValidator :: () -> Integer -> ValidatorCtx -> Bool
    mkValidator () r _
        | r == 42   = True
        | otherwise = False
        
 Note: We will see that ValidatorCtx will be changed to ScriptValidator in new Plutus releases. 
 
 In this function we have: Datum :: unit; Redeemer :: Integer; Validator :: ValidatorCtx
 
 As the validtor is expecting Data as data type we need to make the "conversion" by using data Typed as follows:
 
     data Typed
    instance Scripts.ScriptType Typed where
        type instance DatumType Typed    = ()
            type instance RedeemerType Typed = Integer

The haskell function needs to be converted into a Plutus validator function to be compiled. 

    validator :: Validator
    validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
    
Combined with the inlinable 
 
     {-# INLINABLE mkValidator #-}
