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
The find function of f is finding the UtXO that contains the token.

Now we have 2 contracts for the 2 players.

```Haskell
firstGame :: forall w s. HasBlockchainActions s => FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (gameInst game) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    void $ awaitSlot $ 1 + fpPlayDeadline fp

    m <- findGameOutput game
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData ClaimFirst)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                         <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ fpRevealDeadline fp)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"
            
            
            
```
Second player contract is similar to first one, except that there is no nonce. 
    
    
## State Machine

Machine in one state that can transition to another state or to a final state.
For example, Player 1 plays is one machine state represented by the datum of that UtXO. Transitions to subsequent states are transactions. 
There are special support in Plutus library in Plutus.Contract.StateMachine.

```Haskell
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s),

      -- | Check whether a state is the final state
      smFinal       :: s -> Bool,

      -- | The condition checking function. Can be used to perform
      --   checks on the pending transaction that aren't covered by the
      --   constraints. 'smCheck' is always run in addition to checking the
      --   constraints, so the default implementation always returns true.
      smCheck       :: s -> i -> ScriptContext -> Bool,

      -- | The 'AssetClass' of the thread token that identifies the contract
      --   instance.
      smThreadToken :: Maybe AssetClass
    }

```
s: State. i:input. Correspond to datum and redeemer respectively.

smFinal are special state in the sense that it does not produce a new state.

```Haskell
data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show
```

Now with the Finished state added to the GameDatum

```Haskell
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)       <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)   <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game) <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing
    
    
```

Transition section is the core of the code. 

Differences with the EvenOdd.hs contract:

Token validation is not needed anymore as state machine does the validation automatically. 

CheckNonce in the old contract is not present anymore because this cannot be expressed in the constraints. So the smCheck can be used. 


Create a StateMachineClient

runInitalize, runs the state machine by taking the client (Datum) (Value)

GetOnChainState takes the client and returns the state (UtXO). 

RunStep creates transaction and submitts to move to the new state. All it needs is the redeemer. 

TestStateMachine.hs is exactly the same test code as before, except that is calling the state machine contract. 
