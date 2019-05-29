# Development

The workflow used in this repo is described in [the haskell-nix guide](https://github.com/Gabriel439/haskell-nix/).

Most notable commands:

```
# Build
nix-build --attr dzen-dhall nix/release.nix
# Enter shell
nix-shell
# Generate the derivation using `cabal2nix`.
# Nix files, except shell.nix, all reside in `nix/` directory:
cd nix && cabal2nix ../ > dzen-dhall.nix
```
