### Week03 requires an upgrade from plutus repo

In ~/plutus
    
    git pull
    git commit 3aa86304e9bfc425667051a8a94db73fcdc38878
    nix build -f default.nix plutus.haskell.packages.plutus-core.components.library


