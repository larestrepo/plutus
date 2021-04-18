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

For the pioneer plutus program is being used GHC 8.10.2 and Cabal 3.4.0.0

    ghcup install ghc 8.10.2
    ghcup install cabal 3.4.0.0

Now set the versions:

    ghcup set ghc 8.10.2
    ghcup set cabal 3.4.0.0
    
Re-check your current GHC and Cabal version. If it's still not the correct version, try to restart your terminal.

### Building Plutus core

Clone the repo plutus core git

    git clone https://github.com/input-output-hk/plutus.git
    cd plutus
    git checkout 3746610e53654a1167aeb4c6294c6096d16b0502

Build Plutus core with Nix

    nix build -f default.nix plutus.haskell.packages.plutus-core.components.library

### Start the plutus playground

Go to Plutus repository and start nix-shell

    cd plutus
    nix-shell

In plutus-playground-client folder start the server

    cd plutus-playground-client
    plutus-playground-server

In other terminal start the Playground client with nix-shell

    cd plutus
    nix-shell

    cd plutus-playground-client 
    npm run start

### Access the playground

it should be running on: https://localhost:8009

This completes the installation of the Playground 

# Running cabal locally

At this point it is only possible to compile plutus in the playground but it should be possible to compile the program locally so we don't need paste it in the playground for compilation. Also we are going to install IDE for coding. Personally I use Visual Studio Code but you can use the one of your preference. 

If not done, clone the repository from the pluts pioneer program

    git clone https://github.com/input-output-hk/plutus-pioneer-program.git
    
This can be located besides the plutus folder previously clone during the playground setup. 

    ~/plutus
    ~/plutus-pioneer-program
    
In the plutus folder start nix-shell

    cd ~/plutus
    nix-shell
    
Then go to Week01 folder and build cabal

    cd ~/plutus-pioneer-program
    cabal update
    cabal build
    
### Install Visual Studio code

Install visual studio code in linux should be straightforward. Install the Haskell extensions and Vim editor if you like.

With nix-shell active go to the project folder i.e. Week02 folder

    code .
    





