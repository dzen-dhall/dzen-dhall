{- This function creates a Source that runs a given bash script with specified update interval.
-}

let types = ../types/package.dhall

let mkBash =
		λ(Bar : Type)
	  → λ(carrier : types.Carrier Bar)
	  → λ(interval : Natural)
	  → λ(input : Text)
	  → carrier.source
		{ command =
			[ "bash" ]
		, input =
			input
		, updateInterval =
			Some interval
		, escape =
			True
		}

in  mkBash
