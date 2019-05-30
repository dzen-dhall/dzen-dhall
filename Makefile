.PHONY: cabal2nix build run find-data

cabal2nix:
	cd nix && cabal2nix ../ > dzen-dhall.nix

build:
	unlink ./result || true # To make hash the same
	nix-build --attr dzen-dhall nix/release.nix

run: build
	./result/bin/dzen-dhall

find-data:
	find ./dhall -type f | sed 's#./##'
