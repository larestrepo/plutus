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
MakeLenses convert datatype into an optic that let's you zoom into the datatype. 

    







    
    
