### Week03 requires an upgrade from plutus repo

Now it is possible to start the playground with different timeout.

    pluts-playground-server -i 120s

In ~/plutus-apps
    
    git pull
    git commit 3aa86304e9bfc425667051a8a94db73fcdc38878
    nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library


- In package `plutus-ledger-api`, `Plutus.V1.Ledger.Contexts.ScriptContext`
    - Contains: `TxInfo` and `ScriptPurpose`
    - Script Purposes: Spending (most important), Minting, Rewarding, Certifying
    - `TxInfo` describes spending transaction
        - Especially: Inputs, Outputs
        - Stuff for forging, rewards, data
        - `txInfoId` = Hash of transaction with all inputs and outputs
- `txInfoValidRange`: Big advantage over Eth: Transaction already fail in wallet
    - Fails without paying fees if tx invalid because of UTxO already spent
    - Never happen: Script runs and then fails
    - Time must be handled deterministically: Added slot range (time interval)
    - Checks before validation:
        - If script runs, time falls in interval of slot range
        - => deterministic
    - By default: Infinity slot range
- txInfoSignatories is the list of public keys that have signed this transaction.
- txInfoData Transactions that spend a script output need to include the datum of the script output. The txInfoData field is a list associating datums with their respective hashes. If there is a transaction output to a script address that carries some datum, you don’t need to include the datum, you can just include the datum hash. However, scripts that spend an output do need to include the datum, in which case it will be included in the txInfoData list.

Definition of slot
- [`Plutus.V1.Ledger.Slot`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Slot.html)

Definition of Interval
    - [`Interval`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Interval):
    
 When building the transaction, it is important to know that the submission cannot be beyond 36hours in the future, which is the time in the future that is certain that there would not be any change in parameters that will affect the slot lenght. 
    
Orouborus uses slots, Plutus uses real time. The conversion is done 

```haskell
type POSIXTimeRange = Interval POSIXTime.
```

It is a type synonym for Interval POSIXTime and we see that Interval is defined by a LowerBound and an UpperBound.

Interval
      ivFrom :: LowerBound a
      inTo   :: UpperBound a
If we drill into LowerBound we see the constructor

data LowerBound a = LowerBound (Extended a) Closure
Closure is a synonym for Bool and specifies whether a bound is included in the Interval or not.

Extended can be NegInf for negative infinity, PosInf for positive infinity, or Finite a.

There are more convenient functions:

member
interval
from
to
always
overlaps
contains
isEmpty
before
after


## Vesting contract

- Send money to script. Beneficiary address can only access it after some time
- Types:
    - Datum has benificiary and deadline
    - No redeemer
    - Context provides remaining info
- Required checks in validator
    - Benificiary signature is in context
    - Timing, tx executed after deadline
- Validator
    - Check that benificiary is in list of pub key hashes of signatures
    - Only know that time is within slot range (dont know exactly which slot)
        - => All slots in range must be after deadline

```haskell
data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum
```
Datatype VestingDatum that takes Payment public key hash from the beneficiary and the deadline

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
```
Validator takes Datum and Script context. Redeemer is not necessary in this case. 2 functions are defined: signedByBeneficiary and deadlineReached

It first defines info with the scriptContextTxInfo. info will be used later.

signedByBeneficiary takes info with the helper function txSignedBy and it takes the public key hash

deadlineReached use the contains function to determine if the deadline(txInfoValidRange info) is after the deadline or in an interval from deadline to infinity (from $ deadline dat).

In this vesting contract, 2 new GHC extensions were added: 
```haskell
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
```
and new imports like: 
```haskell
import           Data.Aeson           (ToJSON, FromJSON)



data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
```

For the endpoints, give and grab are still useful in this case. Giveparams provides the endpoints for give: PubkeyHash Beneficiary, Deadline and the amount.

Grab does not need endpoints, as all the conditions to grab are handled by the validator and no additional information is needed to be provided.

The offchain part is similar to previous example like IsData script; the only difference is that this time we need to pass datum as parameter. 

For the grab, we need to filter the utxos suitable in this line:

```haskell
utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
```
where the isSuitable function takes the pubkeyhash and now time to check if the utxo is valid to me. 

```haskell
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> beneficiary d == pkh && deadline d <= now
  ```
**_ciTxOutDatum** determines if the datum is either DatumHash or Datum. If it is a hash, this is the left case and we cannot do anything because we cannot know from the hash the datum, so the result of the function is False. If it is Datum (right case), then with fromBuiltinData is deserialized to Actual Data. If Nothing then is False. if succeeds check that the pubkey hash is beneficiary and the deadline has been passed. 

Once the utxos are filtered...

```haskell
if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
            
 ```
 Constaint the utxos with unspent utxos and with the validator. tx is built with the oref and unitRedeemer (special variable in Plutus where the redeemer is unit and this can be used). 
 
 ## Within the playground
 
 To know the pubkeyhash of the wallet of the give destination, we need to go to the prelude and import wallet.Emulator. Then call the function Knownwallet 1, 2 or 3. And with the mockWalletPaymentPubKeyHash we can finally get the pub key hash of the simulated wallet:
 
    mockWalletPaymentPubKeyHash $ knowWallet 3
    
To determine the deadline, this was already explained how to make slot to time conversion 

Idea now is to parametized context which gives family of scripts instead of 1 script (same hash), as it is instantiated with different params ( diff scripts and addresses)

```haskell
--Before
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool

--Datum parameterized
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool

```
Now the typedValidator takes an argument
```haskell
--before
typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()
 
 ---parameterized
 typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()
 ```
 

Datum becomes a generic parameter and we add an additional param. But the problem is that this param is not known at compile time.

- Solution: Use `PlutusTx.applyCode`
- With the liftCode function that takes a parameter and turns it into Plutus script code.
- Different chose of parameters gives different scripts hashes. 

