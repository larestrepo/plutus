Various ways to test code. The example here is for selling tokens.
In the TokenSale contract:

```Haskell
data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsNFT    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
```

TokenSale is the parameter to be used for the contract and it has 3 fields: tsSeller with the public key hash. tsToken to be sold. tsNFT used to identify the correct UTxO.
As redeemer:

```Haskell
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    deriving (Show, Prelude.Eq)
```

SetPrice in lovelaces; AddTokens, argument to give the number of tokens to add, BuyTokens argument to give the amount of tokens to buy, withdraw with 2 arguments: first, how many tokens to withdraw and second, how many lovelaces to withdraw.

transition to handle several actions and provide constraints: SetPrice, AddTokens, BuyTokens, Withdraw.

SetPrice: constraints must be signed by the seller and it has to be greater than zero. 
The section v <> nft (negate 1) is needed because the library keeps adding it to the transaction? on the right hand side, but the NFT is supposed to stay in the contract forever. 

AddTokens: no mustbesigned by the seller (anyone would be able to add tokens to the contract but that would be a gift). That's why the mempty from the monoid class. State p is the same price, then substract the NFT in the value and finally the tokens to be added

BuyTokens: mempty as anyone can buy. State p, just the price. Negate the nft. Substract the tokens that were there and finally add the lovelaces worth it. 

Witdraw: both non-negative values (n, tokens; l, lovelaces). The seller has to sign otherwise anyone can take the tokens. State p as it is. substract the nft. Substract the tokens and the lovelaces. 

```Haskell
{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v <>
                                                      nft (negate 1)
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    _                                       -> Nothing
  where
    nft :: Integer -> Value
    nft = assetClassValue (tsNFT ts)
```

```Haskell
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) (const False)
```

Build the state machine is easier in this case. The nft, the transition previously defined and whether the state is final. In this case there is no final state. 

```Haskell
startTS :: HasBlockchainActions s => Maybe CurrencySymbol -> AssetClass -> Contract (Last TokenSale) s Text TokenSale
startTS mcs token = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    cs  <- case mcs of
        Nothing  -> C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
        Just cs' -> return cs'
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsNFT    = AssetClass (cs, nftName)
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
    return ts
```

Maybe CurrencySymbol, if nothing it will mint the NFT token associated to the contract for the first time or the NFT currency symbol if already present. 
AssetClass is the token. 
Contract (Last TokenSale), so other contracts can find it. 
Finally return the token sale. 

The contract also has functions for the other actions: set price, add token, buy token, withdraw. 

# Tasty

https://hackage.haskell.org/package/tasty

in Plutus there's special support. Can be found in Plutus.Contract.Test. 

1) Checking predicates.

There is a TracePredicate where several function tests exist. For example, WalletFundsChange: Check that the funds in the wallet have changed by the given amount, excluding fees. 

In the Trace.hs file:

```Haskell
tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token sale trace"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
    )
    myTrace
```

It uses the checkPredicateOptions. Use walletFundsChange to verify what the balance of each wallet is expected. 

# Optics-Lens

When leading with Lenses is convention to called fields with leading underscore (ex. _staff).
In the Lens.hs, there are datatype definitions which can be defined with simple Haskell, but it can become messy. To set city in datatype with goto function it would be like:

```Haskell
goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}
```
Wit makeLenses it is easier to work with datatype, access and modify them.

```Haskell
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
```
MakeLenses convert datatype into an optic that let's you zoom into the datatype. With the & symbol is possible to update the datatype. 

# Property base testing

import Plutsu.Contract.Test and Plutus.Contract.Test.ContracModel

Unit test is a special case from Property base tests. 

By calling the function with quickCheck it is possible to test it. 

quickCheck <name_of_the_function>. QuickCheck not only provides info whether the test pass or not but also provide a counter example where the function fails. 
It makes a number of tests and also shirnks the examples to provide the simpler one. The tests are taken from random generation examples that increase in complexity. By default,
it uses 100 test cases, but this can be configured specially to test in production environments where 1000 or 10000 cases are more appropiate. 

In the Model.hs

To test the token state. 

```Haskell
data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show
 ```
 
 It will test the price, total lovelace and the token.
 
 instance ContractModel TSModel where

```Haskell
    data Action TSModel =
              Start Wallet
            | SetPrice Wallet Wallet Integer
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
        deriving (Show, Eq)
```
Define actions which Quick Check can generate.
    
```Haskell
    arbitraryAction _ = oneof $
        (Start <$> genWallet) :
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]
 ```
 
oneof is special class in Quick check to randomly pick one of the actions to test. 
The rest of the code above is notation monad (anlogy to for loops). It generates random wallet using genWallet with the elements function also from Quick check as specified in line 198. Then it start as defined in the action TSModel part. For example for SetPrice action, it takes 3 random parameters as specified in Action list (2 wallets and amount). The genNonNeg is another function defined in line 200.

 ```Haskell
 initialState = TSModel Map.empty
 ```
 Initial state is empty as no token sale has started yet. 
 
 
 NextState when the model start. Wallet 1 loses his token or gives the token to the contract (withdraw). Then tsmodel maps w and the staet shoule be 0 price, 0 tokens, 0 lovelaces. Then wait 1 timeslot.
 
 NextState when SetPrice. Check that the wallet owns the token is the one that set the price. Only if that happens do something. Funds don't move, but the model is updated. 
 ix v is similar to at w, except that at returns a maybe. Then it looks at the tssPrice lens and set it to p. 
 
 NextState when Addtokens. Given the TsModel and wallet get the model state as defined in line 147 getTSState. Based on the getTSState define if has started as defined in the line 155 hasStarted. If has started check if the tokens are positive. The it uses askModelState to ask something specific to the model state. The reason is to make a balance check. So the tokeamount + the balance change must be greater or equal to n. The amount of tokens. Then it withdraws n amount to the contract. 
 
  NextState when BuyTokens. W wants to buy n tokens from wallet V. First check n is positive. Then get state to confirm that state is not nothing. t is now the token sale state. Check how many tokens are available and n must be lower. p brings the price from tssPrice and the price to pay. deposit the tokens to w wallet. substract one lovelace and finally update the state. 
  
NextState when Withdraw. Only possible if the wallet withdraw is the same running the token sale. Don't withdraw more than what is actually there. Then deposit the amount and update the state. 

```Haskell
nextState (Start w) = do
        withdraw w $ nfts Map.! w
        (tsModel . at w) $= Just (TSState 0 0 0)
        wait 1
 nextState (SetPrice v w p) = do
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (AddTokens v w n) = do
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- askModelState $ view $ balanceChange w
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (Withdraw v w n l) = do
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1
 ```
 Perform function: it will link this code to the actual contract. 
Precondition are conditions that I allow for the actions. 
 
 ```Haskell
    perform h _ cmd = case cmd of
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (nftCurrencies Map.! w, tokenCurrencies Map.! w, tokenNames Map.! w) >> delay 1
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                                               >> delay 1

    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v
 ```
 
 prop_TS: it has the propRunActionsWithOptions. 
 
 ```Haskell
prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]
```

Connect the quick check with tasty by using testProperty

```Haskell
tests :: TestTree
tests = testProperty "token sale model" prop_TS
```



    
    
