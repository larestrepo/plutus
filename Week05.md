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
There's a function called lovelaceValueOf which takes an integer and returns a Value.

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

It takes the MintParams and returns the contract. It takes 4 parameters as it's been seen in previous lecture.
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
In this case, the contract has 2 inputs PubKeyhash (Redeemer) and the context. It means that it must be signed to mint or burn tokens.
ScriptContextTxInfo in our ScriptContext (ctx) contains all the signatories of the transaction; by using txSignedBy we can check if our PubKeyHash is in this list.
txSignedBy takes the TxInfo and a PubKeyHash and returns a boolean.

```Haskell
txSignedBy :: TxInfo -> PubKeyHash -> Bool

policy :: PubKeyHash -> Scripts.MonetaryPolicy
policy pkh = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
```
This is changed because we are parameterizing the pubKeyHash

```Haskell
mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.monetaryPolicy $ policy pkh
        tx      = Constraints.mustForgeValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)
```

To get the pubKeyHash of our contract, we'll use the pubKeyHash function together with our Contract.ownPubKey.

Using the <$> operator, which is just a synonym for fmap

## NFT

Without Plutus, NFTs are not really NFTs. Option 1 is to mint only 1, but this is a restriction only for the transaction. Option 2 is to define a deadline. 

With Plutus we can attach the UTxO which is really unique associated to the NFT, hence really unique. By only allowing one UTxO to mint, we will have a true NFT.
A transaction with 0 input and only outputs without values could not be unique; except for the fees which makes them really unique.

For an NFT contracct the pubkeyhash is replaced by TxOutRef.

```Haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool
mkPolicy oref tn ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                        traceIfFalse "wrong amount minted" checkMintedAmount
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False
        
        
 ```
 hasUTxO section. To validate if the UTxO provided is in the parameter provided in the script. It uses the haskell function any which return true if it can find it. False which goes to the exception.
 
 checkMintedAmount. To limit the amount of tokens created to only 1. It uses the flattenvalue function which converts map to list of the form (symbol, tokenname, amount).
 But the symbol is the hash only known at compile time. But there's a function called 'ownCurrencySymbol', then (ownCurrencySymbol cts, tokenname, amount)
 
 ```Haskell
 type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" TokenName
 ```
 No need to parameters as only the TokenName is needed. 
 
```Haskell
mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

```













