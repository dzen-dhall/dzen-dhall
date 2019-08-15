.PHONY: cabal2nix build run

cabal2nix:
	cd nix && cabal2nix --no-haddock ../ > dzen-dhall.nix

build: cabal2nix
	unlink ./result 2>/dev/null || true # To make hash the same
	nix-build --attr dzen-dhall default.nix

run: build
	./result/bin/dzen-dhall
