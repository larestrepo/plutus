# Lecture 05 is all about native tokens

We'll be looking at how to Mint and Burn tokens, also how to define minting policies; which define under which conditions native tokens can be minted or burned.

## Tokens are identified by CurrencySymbol and TokenName. The combination of these 2 is called AssetClass.

In other words:

```Haskell
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
```

And the value is given by these 2 as well:

```Haskell
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
```
There's a function caled lovelaceValueOf which takes an integer and returns a Value.

To create Values for tokens other than ADA, we can use singleton

```Haskell
singleton :: CurrencySymbol -> TokenName -> Integer -> Value

singleton "a8ff" "TOKEN" 10
```
Would return: Value (Map [(a8ff,Map [("TOKEN",10)])])

ValueOf is used to extract the value of a token. Flattenvalue is to convert map to list.

```Haskell
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
valueOf v "a8ff" "XYZ"
flattenValue v
[(a8ff,"ABC",7),(a8ff,"XYZ",100),(,"",42)]
```

## Minting Policy

Minting Policies only get the ScriptContext from the Validator.
Datum and Redeemer would both not make sense; 
the Datum belongs to a UTxO, and the Redeemer to input, but the Forging (txInfoForge) belongs to the transaction, not to a specific Input or Output.

```Haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptContext -> Bool
mkPolicy _ = True

policy :: Scripts.MonetaryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])
```

The simplest Validator Script, which takes the context and returns always true.

To create the currency symbol:

```Haskell
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
```

Now the off-chain part. We create an endpoint with paramaters amount and tokenname to be able to burn or create tokens.

```Haskell
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams
 ```
 
 The contract itself as follows:
 
 ```Haskell
 mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.monetaryPolicy policy
        tx      = Constraints.mustForgeValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)
```

It takes the MintParams and returns the contract. It takes 4 parameters.
- w: Tell/Writer is not used. (written in lower case to indicate is not used)
- FreeSchema: (Blockchain actions) which access to the Endpoint previously defined
- Text: for erro messages 
- () returns nothing

mint mp = do... as it is monad do statement can be used. 

set the val variable with the token created by using the singleton function which takes 3 arguments 

About tx. Instead of creating the transaction by hand, we use Constraints to define conditions that the transaction must meet. All the constraints starts with must (mustForgeValue, mustPayToPublicKey, etc)
Constraints will take care of all the tx calculations like fees and so on. 

submitTxConstraintsWith is used to send the tx to the chain. It uses the lookups to tell him where to find the Policy which is above. Other possible lookups are: UTxOs, validation scripts, etc.
@Void: we don't care about the redeemer
Then bind the result to the ledgerTx 

After submission, we wait for confirmation and then log message that the token was forged.

```Haskell
endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
    where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []
```

Boilerplate function to make it available in the playground.

## A more realistic example.

```Haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh
```




