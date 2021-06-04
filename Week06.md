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
