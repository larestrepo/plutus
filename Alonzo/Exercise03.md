Download the pre-built AlwaysSucceeds.plutus Plutus script, from https://github.com/larestrepo/Alonzo-testnet/blob/main/resources/plutus-scripts/AlwaysSucceeds.plutus 

All transaction outputs that are locked by non-native scripts must include the hash of an additional “datum”. A non-native script-locked output that does not include a datum hash is unspendable
  
  Datum Hash
  
    DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value <any number>)
  
  Generate script address
  
    cardano-cli address build --payment-script-file <path to the Plutus script file> --testnet-magic 7 --out-file script.addr
  
 Where payment-script-file is the AlwaysSuccedes.plutus file:
 
 ```Haskell
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "585c585a010000332233322233333322222233222220051200120012122222300500622122222330040070062122222300300621222223002006212222230010062001112200212212233001>
}
```
 
 Query protocol params
 
    cardano-cli query protocol-parameters --testnet-magic 7 > pparams.json 
  
Send funds to the script address, we must include the datum hash and the script.addr previously generated
  
    cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in <UTXO position of the sender address> \
    --tx-out $(cat script.addr)+900000000 \
    --tx-out-datum-hash $DATUM_HASH \
    --tx-out <sender address>+$CHANGE \
    --fee 1000000 \
    --protocol-params-file pparams.json \
    --out-file tx.raw
  
  For now fee is fixed at 1ADA.
  
  tx.raw output looks like:
  
  ```Haskell
  {
    "type": "TxBodyAlonzo",
    "description": "",
    "cborHex": "85a50081825820dcff97d55818b5a9946997aef41a46fc54ccff0d1d5e7bedb5915c90eca07722000d80018283581d708a08f851b22e5c54de087be307eeab3b5c8588a8cea8319867c786e01a35a4e9005820a8884d357d7505108a35e2705d8b3e35721bc10ceeeab5f890bf2710ad007e2d825839009681cb747c011d395676fe31492db6b19e52adfc8f38a1da94ee555db9ecb4ce9f109b9256565fdd931851d87421a330617c336d405e62121a05e69ec0021a000f42400e809fff8080f6"
}
```  
  
  Finally transaction sign and transaction submit
  
    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file <senders wallet>.skey --testnet-magic 7 --out-file tx.sign
    cardano-cli transaction submit --testnet-magic 7 --tx-file tx.sign
  
You should get: "Transaction succesfully submitted" message.

Query the script address balance:

    cardano-cli query utxo --address $(cat script.addr) --testnet-magic 7 | grep $DATUM_HASH


