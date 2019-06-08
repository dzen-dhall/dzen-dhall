.PHONY: cabal2nix build run update-data-files

cabal2nix:
	cd nix && cabal2nix ../ > dzen-dhall.nix

build:
	unlink ./result 2>/dev/null || true # To make hash the same
	nix-build --attr dzen-dhall default.nix

run: build
	./result/bin/dzen-dhall

test: update-data-files
	stack test

update-data-files:
	./misc/update-data-files.sh
