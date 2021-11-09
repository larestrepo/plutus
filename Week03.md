### Week03 requires an upgrade from plutus repo

Now it is possible to start the playground with different timeout.

    pluts-playground-server -i 120s

In ~/plutus
    
    git pull
    git commit 3aa86304e9bfc425667051a8a94db73fcdc38878
    nix build -f default.nix plutus.haskell.packages.plutus-core.components.library


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

Definition of slot
- [`Plutus.V1.Ledger.Slot`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Slot.html)

Definition of Interval
    - [`Interval`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Interval):
    
    
## Example: Vesting

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

