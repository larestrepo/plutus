1. Create application folder

        mkdir <Folder_Name>

        cabal init --cabal-version=2.4 --license=NONE -p <Folder_Name>

1. Create cabal.project and modify myplutus.cabal files

1. Activate nix-shell from plutus-app

1. In myplutus folder 

        cabal update
        cabal build
        cabal repl
