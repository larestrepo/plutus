Download the pre-built AlwaysSucceeds.plutus Plutus script, and obtain the script address.

  
  Datum Hash
  
    DATUM_HASH = $(cardano-cli transaction hash-script-data --script-data-value <any number>)
  
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
  
  Finally transaction sign and transaction submit
  
    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file wallet2_payment.skey --testnet-magic 7 --out-file tx.sign
    cardano-cli transaction submit --testnet-magic 7 --tx-file tx.sign
  
