{- This function creates a Source that runs a given bash script with specified update interval.

Additionally, it asserts that given executables are in PATH.
-}

let prelude = ../prelude/package.dhall

let types = ../types/package.dhall

let Assertion = types.Assertion

let Carrier = types.Carrier

let mkBash = ./mkBash.dhall

let mkBashWithBinaries =
		λ(Bar : Type)
	  → λ(carrier : Carrier Bar)
	  → λ(binaries : List Text)
	  → λ(interval : Natural)
	  → λ(input : Text)
	  → carrier.join
		[ carrier.join
		  ( prelude.List.map
			Text
			Bar
			(   λ(binary : Text)
			  → carrier.check "" (Assertion.BinaryInPath binary)
			)
			binaries
		  )
		, mkBash Bar carrier interval input
		]

in  mkBashWithBinaries
