# This is about Marlowe. 

Marlowe is a DSL (domain specific language) for finacial contracts. 

To avoid safety issues: Marlowe contracts are finite, will terminate, it has a defined timeline, no assets retained on close.

A contract in Marlowe is represented as a Haskell datapype with a simple set of constructs as shown below. This can be found in semantics.hs as part of plutus library and specifically in 
plutus/marlowe/src/Language/Marlowe/Semantics.hs


```Haskell
data Contract = Close
    | Pay Party Payee Value Contract
    | If Observation Contract Contract
    | When [Case Action Contract]
      TimeOut Contract
    | Let ValueId Value Contract 
    | Assert Observation Contract
```



