
> The following is taken from [https://nixos.org/guides/nix-pills]

# Installation

    curl -L https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

Install something to test

    nix-env -i hello
    nix-env --list-generations
    nix-env -q
    nix-env --rollback
    nix-env -G 3
    nix-store -q --references `which hello`
    nix-store -q --referrers `which hello`

### Closures
The closures of a derivation is a list of all its dependencies, recursively, including absolutely everything necessary to use that derivation.

    nix-store -qR `which man`

Copying all those derivations to the Nix store of another machine makes you able to run man out of the box on that other machine. That's the base of deployment using Nix, and you can already foresee the potential when deploying software in the cloud (hint: nix-copy-closures and nix-store --export).

A nicer view of the closure:

    nix-store -q --tree `which man`