# This repo contains instrunctions to setup the environment for plutus as part of the plutus pioneer program training.

### HW recommendations:

    Ubuntu on VPS, virtualbox or WSL
    At least 30GB harddrive
    Decent RAM

### Installing NIX

    sudo apt update
    sudo apt upgrade -y

Make Single user installation. For details go to https://nixos.org

        sh <(curl -L https://nixos.org/nix/install) --no-daemon
        
 Do not forget to source the path as recommended at the end of the installation: 
 
    . /home/<use_name>/.nix-profile/etc/profile.d/nix.sh
        
Edit nix.conf file to decrease the amount of time for building.
In case of not having /etc/nix/nix.confg create ~/.config/nix/nix.conf instead. Add the following lines:

      substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
      trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
      
Confirm that the changes were taken by running

    nix show-config
      
### Installing GHC and Cabal

      curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
      
 Follow the instructions and install in separate command line the dependencies.
 
 Normally the dependencies are:
 
    sudo apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
 
 Check GHC and Cabal version
 
     ghc --version
     cabal --version

For the pioneer plutus program I am using  GHC 8.10.7 and Cabal 3.4.1.0

    ghcup install ghc 8.10.7
    ghcup install cabal 3.4.1.0

Now set the versions:

    ghcup set ghc 8.10.7
    ghcup set cabal 3.4.1.0
    
Re-check your current GHC and Cabal version. If it's still not the correct version, try to restart your terminal.

### Building Plutus app

Important to checkout the tag for the week to be compiled; this can be found in the plutus-pioneer-program repo, in the cabal.project file for each week.

Clone the repo plutus core git

    git clone https://github.com/input-output-hk/plutus-apps.git
    cd plutus-apps
    git checkout 41149926c108c71831cfe8d244c83b0ee4bf5c8a

Build Plutus PAB library core with Nix

    nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library

### Start the plutus playground

Start nix-shell

    nix-shell

In plutus-playground-client folder start the server

    cd plutus-playground-client
    plutus-playground-server

In other terminal start the Playground client with nix-shell

    nix-shell

    cd plutus-playground-client 
    npm run start
 
 ### Build plutus documentation
 
 In other terminal in the plutus-apps folder
 
     nix-shell
     build-and-serve-docs

### Access the playground

it should be running on: https://localhost:8009

This completes the installation of the Playground 

# Running cabal locally

At this point it is only possible to compile plutus in the playground but it should be possible to compile the program locally so we don't need to paste it in the playground for compilation. Also we are going to install IDE for coding. Personally I use Visual Studio Code but you can use the one of your preference. 

If not done, clone the repository from the plutus pioneer program

    git clone https://github.com/input-output-hk/plutus-pioneer-program.git
    
This can be located besides the plutus folder previously cloned during the playground setup. 

    ~/plutus
    ~/plutus-pioneer-program
    
In the plutus folder start nix-shell

    cd ~/plutus
    nix-shell
    
Then go to Week01 folder and build cabal

    cd ~/plutus-pioneer-program
    cabal update
    cabal build
    
 Now you can compile in your terminal the contract. Typical commands are:
 
    cabal repl -> starts the cabal compiler
    :l src/Week02/<name of the module> -> loads the module
    .r -> compiles and check for errors
    :t -> search command
 

Happy coding.





