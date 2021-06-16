### State machines

We will implement in Plutus the Even-Odd game. 2 players one tells a number, the second tells a number. If the sum is Even player 1 wins, if the sum is Odd player 2 wins.
They have to submit the number encrypted with a nonce via a hash so there's no way to know in advance the number given by each of the players.

EvenOdd.hs

```Haskell
data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !Slot
    , gRevealDeadline :: !Slot
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
 ```
 
 First we define the datatype Game. gFirst and gSecond are the hashes containing the number. gStake is the amount in play. gPlayDeadlline and gRevealDeadline are the deadline time for play and reveal.
 gTokenm arvbitrary NFT to identify the right instane of the UTxO to keep track where we are in the game. 
 
 ```Haskell
 
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)
```

Type GameChoice. The 2 moves that players can make. Prelude.Eq and Prelude.Ord: normal Haskell which is not possible in plutus. So it has to be done by hand defining == and using inlinable in Haskell for this to work.

```Haskell
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False
    
data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
```

And GameDatum is the state of the contract. ByteString is the Hash that the first player submits. (Maybe GameChoice) is the hash of the second player. Then after the equality verification.

```Haskell
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show
```
DataType game redeemer with all the possible options:

Play GameChoice: Play with argument GameChoice
Reveal ByteSTring: Reveal the nonce.
ClaimFirst: First player can claim
ClaimSecond: SEcond player can claim

```Haskell
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
```
Validator
- Game are parameters of game type
- Two ByteString are represenations for choice 0 and 1
    Because can not use bytestring literals in Plutus
- Then the usual Datum, Redeemer and context. 
- checkNonce
    First ByteString is submitted hash
    Revealed nonce
    Choice of 2nd player (must be same as choice of 1st, otherwise first player does not win)
    Check hash
   
   ```Haskell
   nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1
   ```
   Token/NFT given back to first player since he initialized the game. 
   
   ## Conditions
   One common is that the input must be validated to the token
   
   Now different conditions depending on player moves.
   
   ```Haskell
   case (dat, red) of
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False
     
    ```
Off-chain code for player 1

```Haskell
findGameOutput :: HasBlockchainActions s => Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !Slot
    , fpRevealDeadline :: !Slot
    , fpNonce          :: !ByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
```

    
    









