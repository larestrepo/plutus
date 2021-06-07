### This lecture is about oracles

Our Oracle value (Datum) will sit a the script address of the Oracle.

We'll need a way to distinguish true UTxO Oracle output from the others sitting at the same script address.

So to make the correct Oracle output unique, is to not only have it carry the data, but also an NFT.

## ADA-USD swap contract

The contracts needs to validate the following:

Validator 1

- the NFT must be present in the UTxO
- make sure there is an output at the same oracle address
- this output must also contain the NFT and the same Oracle value
- the Fee must be payed

Validator 2

Swap validator. 

It consumes the UTxO of the Seller (containing the ADA), the Buyer UTxO with the USD and the fees for the Oracle, 
and will then (if everything checks out) give the USD to the Seller, and the ADA to the Buyer.

But the exchange rates, change over time

The transaction doing the update, must consume the UTxO and provide a new one with the updated value and it must be signed by the Oracle provider (we wouldn't want just anybody changing the Oracle value) and it should also hold the same NFT of course.

This also gives us the possibility to collect all the fees that were payed by users of this Oracle.

```Haskell
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oFee      :: !Integer
    , oAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
oSymbol -> the CurrencySymbol of our NFT. We don't need the TokenName, as we'll just leave this empty.
```

oOperator -> the public key hash of the owner/operator of the Oracle (can do updates, and collect the fees)

oFee -> the fees in lovelaces for the usage of the oracle data

oAsset -> what to exchange for our ADA (in this case this would be USD (represented as a Token, since USD of course does not exist on the Blockchain))

data OracleRedeemer = Update | Use

We want to support our to functions update and use, so our OracleRedeemer can be either.

```Haskell
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d
    
```
From the TxOut of the contract, it gets the datum and finally convert it into an integer.
Why integer and not float or real number? Because there is an issue with the ratio type, so it is needed to multiply by 100000 and then round it. 
Is a 3 step process to get the value from the TXOut associated with the contract. from the txout datum it gets the datum hash if succeedes. Turn datum hash into datum and 
finally from datum get the integer. 

```Haskell
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
```

Oracle -> parametrization of our Oracle Data type

Integer -> Our Datum, in this case the current exchange rate

OracleRedeemer -> use or update

ScriptContext -> should be clear by now

```Haskell
traceIfFalse "token missing from input"  inputHasToken  &&
traceIfFalse "token missing from output" outputHasToken &&
```

For both use or update, these 2 checks are the same, which is why we do them upfront.

First we check that we have input that holds the NFT (inputHasToken), and the same for our output (outputHasToken)

```Haskell
ownInput :: TxOut
ownInput = case findOwnInput ctx of
    Nothing -> traceError "oracle input missing"
    Just i  -> txInInfoResolved i
```

Retrieves the Oracle input using findOwnInput on our ScriptContext, or returns an error if none is found.

```Haskell
inputHasToken :: Bool
inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1
```

Checks if the NFT exists exactly once in the UTxO input (ownInput), using assetClassValueOf which returns the amount of times the asset is present.

```
ownOutput :: TxOut
ownOutput = case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one oracle output"
```
Using getContinuingOutputs we get a list of all the outputs that go to the same script address that we are currently validating.

Since we expect/want only 1 output, using [o] we imply that only when there is 1 item in the list, we return it, otherwise _ we throw an error.

```Haskell
outputHasToken :: Bool
outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1
```
Checks if the NFT exists exactly once in the UTxO output (ownOutput).

Now for the both Use | Update cases

```Haskell
case r of
    Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                traceIfFalse "invalid output datum"       validOutputDatum
    Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                traceIfFalse "fees not paid"              feesPaid
```

For update, we check if the operator signed the transaction by using txSignedBy.

And if the output datum (in this case our Exchange rate) is valid by using validOutputDatum defined on lines 106 - 110

```Haskell
outputDatum :: Maybe Integer
outputDatum = oracleValue ownOutput (`findDatum` info)

validOutputDatum :: Bool
validOutputDatum = isJust outputDatum
```
For the update part of the script, we are only interested in checking if it is a Integer, for the use function though, we also want to check if the Datum HASN'T changed.

The use function can be called by anybody, and thus is much more restrictive in that it of course does not allow for any changes to the Datum (exchange rate).

Which is what we check with outputDatum == Just x (where x is our input datum).

Last but not least, when calling the use function, fees must be paid to the Oracle for providing the data.

We check if they are paid with feesPaid, defined on lines 112 - 118

```Haskell
feesPaid :: Bool
feesPaid =
    let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
    in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))
```

Checks if the outputs are greater than or equal to the inputs + fees.

## start the oracle

```Haskell
startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle
```

startOracle, does what it says it'll do :D

Here we are forging the NFT which will be used throughout all functions.

We could of course do this with the code we wrote in week 05

But this time we'll use the forgeContract function, with our Contract Public Key to forge our NFT.

mapError is used here, because we want to map the Contract monads error messages to Test, instead of in this case CurrencyError.

```Haskell
updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x
```
updateOracle needs to handle the case when there is no UTxO yet, we do so by calling findOracle
```Haskell
findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1
```
Basically checks if at the script address a UTxO with NFT exists, if not return Nothing.

updateOracle then continues to either create the UTxO at the address with the first exchange rate, (let c) mustPayToTheScript with the NFT and Datum.

So we create a new Transaction (submitTxConstraints), and log that we have set the initial oracle value.

If it does exist already, the existing UTxO must be spend (because we cant "change" an existing UTxO, we can only spend the existing one, and create a new one with the updated value).

We'll also create a new transaction here, only with one more constraint.

The first Constraint is the same as before (let c) mustPayToTheScript, because we want to create a new UTxO, only this time we also want to spend the current Existing UTxO, which we'll do by adding the mustSpendScriptOutput constraint.

To find the current UTxO we use the lookup unspentOutputs, which together with Map.singleton returns just one output.

Now since we have an input of the NFT AND the Fees that we've collected from the users of the Oracle.

But our new Output UTxO only contains the NFT.

When we call the mustSpendScriptOutput, it will create an imbalance, the balancing algorithm will automatically balance the transaction, and transfer all user paid fees to the wallet of the operator.

## Run the oracle

runOracle defined on lines 188 - 200

```Haskell
type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
```

Creates the endpoint update, starts the oracle (which as why explained earlier, mints the NFT), and writes the Oracle value (our exchange rate).

We then use tell to communicate our just created Oracle to the outside world.

Since tell requires a monoid, we use Last.

Last is a monoid operation that remembers our last Just value.

Small snippet which explains Last really nicely.

Last (Just 'x') <> Last (Just 'y')
    > Last { getLast = Just 'y' }

Last (Just 'x') <> Last Nothing
    > Last { getLast = Just 'x' }

Nothing <> Last Nothing
    > Last { getLast = Nothing }
At the end it'll call go oracle again. (infinite loop).

## Swap contract

The Swap contract will be using our Oracle.

Open the file Swap.hs plutus-pioneer-program/code/week06/src/Week06/Swap.hs

We will put ADA in the smart contract, which somebody can then exchange for another token (USD) (Since there is no USD on the blockchain, we will use USDT as a USD Token).

The exchange rate will be determined by the Oracle, which may change over time (obviously).

We'll create the validator mkSwapValidator.

Lines 46 - 51

```Haskell
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)
Oracle -> from the Oracle module
```

Address -> the Oracle Address.

Note: Normally we could compute the oracle address with a function on our first parameter Oracle, but this is not doable in this Validator, it can't be compiled into Plutus, so we have to explicitly give our Validator the Oracle Address.

Our Datum is the PubKeyHash of the Seller.

And for the Redeemer we have a value of type Unit (Void).

Now first we check if the Transaction is signed by the seller. If so: the next two checks are obsolete.

Otherwise, we'd first check if we have exactly 2 script inputs (thus avoiding interference with other smart contracts). Defined on lines 74 - 79

```Haskell
hasTwoScriptInputs :: Bool
hasTwoScriptInputs =
    let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
    in
        length xs == 2
```

Filter for script inputs and check if there are exactly 2.

We'd also check if the seller gets paid. Lines 90 - 96

```Haskell
sellerPaid :: Bool
sellerPaid =
    let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
    in
        pricePaid >= minPrice
```

We sum up all outputs that go to the public key address with valuePaidTo info pkh.

Then we compare this to minPrice, which is defined on lines 81 - 88

```Haskell
minPrice :: Integer
minPrice =
    let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
    in
        price lovelaceIn oracleValue'
```
Which gets how may lovelaces are locked in the Swap.

Also in our where, the first few lines (57 - 72) gets the Oracle Input (the exchange rate).

```Haskell
oracleInput :: TxOut
oracleInput =
    let
        ins = [ o
            | i <- txInfoInputs info
            , let o = txInInfoResolved i
            , txOutAddress o == addr
            ]
    in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

oracleValue' = case oracleValue oracleInput (`findDatum` info) of
    Nothing -> traceError "oracle value not found"
    Just x  -> x
```
With some error handling when we get more than one input, or no value at all.

## Offer Swap Contract
Defined on lines 118 - 124
```Haskell
offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (swapInst oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
```

The offer swap contract is for the seller if he wants to provide a swap (ADA -> USD(T))

It takes as parameters the Oracle, and an Integer, which is the amount of lovelaces the seller wants so put up for swapping.

Basically all this contract does, is Pay lovelaces to the script.

## Helper function: findSwaps
Defined on lines 126 - 141
```Haskell
findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        PlutusTx.fromData d

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)
```
This function will return all the swaps, that fulfill a predicate (PubKeyHash -> Bool). (this will be used in the next couple functions, for getting the correct UTxOs)

mapMaybe is defined as mapMaybe :: (a -> Maybe b) -> [a] -> [b], this function basically filters a list based if the value is Just or Nothing.

An example:

f (n :: Int) = if even n then Just (div n 2) else Nothing
f is a function that takes a Integer, which on an even number, will return a Just value of that Integer divided by 2, otherwise it will return Nothing.

Combined with mapMaybe, we could run it as

mapMaybe f [2, 4, 10, 11, 13, 100]
Which would then return [1, 2, 5, 50].

So in this case it filters the UTxOs at the swapAddress of the oracle, with f returning the PubKeyHash (or failing if we cant get it), and g then running it against our predicate. We are using guard for this, if the predicate returns True, guard won't do anything, but when it's False, guard will stop right there (i.e. guarding for a False statement).

## retrieveSwaps contract

Defined on lines 143 - 155
```Haskell
retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"
```
retrieveSwaps will return all the Swaps that belong to us.

We use the findSwaps function that we've defined earlier; with the predicate (== pkh), in other words, return all the swaps where the pubKeyHash equals our ownPubKey.

If findSwaps returns none, just log "no suitable swap found", otherwise, create a transaction that returns all of our swaps to us.

## useSwap contract
Defined on lines 157 - 193
```Haskell
useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toData x)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt
```
This is the interesting contract, as this one will make use of the Oracle.

We start with the ownFunds function (we'll define this function later on in the Funds module), that just adds up all funds of the one calling useSwap, which we then filter with assetClassValueOf to return all of our oAsset oracle (in this case the USD Token).

We then try to find the Oracle (the UTxO that contains the Oracle and the Value (Exchange rate)) with findOracle (defined earlier).

We then search for all swaps that we are NOT the owner of (predicate /= pkh).

And try to find one that we can afford (one that we have enough funds for to swap with) with find (f amt x) swaps.

In real life of course this wouldn't make much sense. You'd probably want to specify a specific amount that you would want to swap, etc. but since this lecture is about the oracles and not a realistic swap contract, we've kept it simple here.

If we do find one we can afford, just take the first one we find.

We'll then construct a transaction to do the swap.

The output of the oracle, will be the existing output, plus the fees we need to pay added to it with <> lovelaceValueOf (oFee oracle).

We then define p which will be the amount we have to pay to do the actual swap by taking the lovelaces contained in the Swap (o') and the exchange rate from the Oracle, and multiply it by using our price function that we defined earlier.

We create some constraints for the transaction:

We'll spend the Oracle output with the Use redeemer (first time using it here, instead of the Update redeemer)

We consume/spend the swap input

We need to pay the Fee to the Oracle

We need to pay the seller of the lovelace (with the price we calculated p)

To make everything work, we need to also add lookups for the swapValidator, the oracleValidator, and the two UTxOs we want to consume (the oracle / the swap)

## Endpoints
Lines 195 - 200
```Haskell
type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()
```
offer -> to offer a swap (with Integer as the amount of ADA we want to offer for swapping)

retrieve -> to retrieve all offered swaps

use -> to do a swap

funds -> will give back my currently available funds

To combine all endpoints, thus calling the correct Contract associated with it.

We'll use select. (Line 203)

swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
Basically offers all endpoints, and executes the first one that is triggered, then recursively calls itself to start all over again.

Lines 205 - 224, will bind all endpoints to the correct contract.
```Haskell
offer :: Contract (Last Value) SwapSchema Text ()
offer = h $ do
    amt <- endpoint @"offer"
    offerSwap oracle amt

retrieve :: Contract (Last Value) SwapSchema Text ()
retrieve = h $ do
    endpoint @"retrieve"
    retrieveSwaps oracle

use :: Contract (Last Value) SwapSchema Text ()
use = h $ do
    endpoint @"use"
    useSwap oracle

funds :: Contract (Last Value) SwapSchema Text ()
funds = h $ do
    endpoint @"funds"
    v <- ownFunds
    tell $ Last $ Just v
```
We wraps all contracts with the error handler h (Lines 226 - 227), that will just log the error, and continue.

h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
h = handleError logError

## Funds Module
We've used the function ownFunds a few times now, basically all this does is return all the funds I have in my wallet.

The ownFunds function is defined in the Funds module (Funds.hs).

Lines 29 - 35
```Haskell
ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v
```
Gets our own public key, all the UTxOs at that key, and adds all the values of all UTxOs together with mconcat, after which we'll log this, and return the total of funds in this wallet.

We also have a small variation on this function, namely ownFunds', which uses the original ownFunds function, only tell's the value instead of returning it.

Lines 37 - 41
```Haskell
ownFunds' :: Contract (Last Value) BlockchainActions Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
```

## Test module

All our code is ready. Ready to test! (the fun part)

File: Test.hs

We'll start by defining our base asset (our USD token)

Lines 36 - 40
```Haskell
assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"
```
Of course the CurrencySymbol would normally be the Hash of a real Currency, but for testing purposes ff will suffice.

Next we use the extended runEmulatorTraceIO -> runEmulatorTraceIO' (Line 43), which will give us more customization properties, specifically the ability to provide wallets with an initial distribution of Tokens.

Lines 45 - 50
```Haskell
emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

v :: Value
v = Ada.lovelaceValueOf                    100_000_000 <>
    Value.singleton assetSymbol assetToken 100_000_000
```
We'll create 10 wallets, with each 100 lovelaces and 100 of our USDT tokens.

Next a helper contract checkOracle, that will periodically (every 1 slot) check the oracle value (exchange rate), and log it. (this is of course very helpful for our test).

Lines 52 - 58
```Haskell
checkOracle :: Oracle -> Contract () BlockchainActions Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle
```
Next we will define our trace! Lines 60 - 111
```Haskell
myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
```
Start the oracle runOracle op with the correct params (amount of fees / our USDT)

use the getOracle helper function, which checks with observableState if the oracle is there, if not, wait for 1 slot and try again.

Initialize the Oracle with the update endpoint with a value (exchange rate) of 1.5 USDT per ADA.

ownFunds' for telling our initial funds

swap oracle to start the swap contract on wallet 3, 4 and 5

Now we're ready for some test scenarios.

Wallet 3 and 4 will offer ADA for swapping (10 and 20).

Wallet 5 uses the swap with the endpoint use.

Note: we don't know which swap will be used, since we've defined the useSwap function to just return the first swap we have the funds for.

Now we'll update the exchange rate to 1.7 USDT for each ADA (with the update endpoint).

And we'll have Wallet 5 use the swap again (endpoint use).

Now we will update the exchange rate to 1.8 USDT, which will automatically have wallet 1 retrieve all the fees paid to the oracle.

If we now run our Trace, the final balances SHOULD look like this:

Wallet 1 -> close to 2 ADA more than before (wallet 1 paid fees to start the oracle)

Wallet 2 -> nothing changed (we didn't do anything with wallet 2)

Wallet 3 -> got USD(T) for an exchange rate of 1.7, paid the corresponding ADA and Oracle usage fees

Wallet 4 -> got USD(T) for an exchange rate of 1.8, paid the corresponding ADA and Oracle usage fees

Wallet 5 -> Paid the USD(T) to wallet 3 and 4, got ADA in return of course.

