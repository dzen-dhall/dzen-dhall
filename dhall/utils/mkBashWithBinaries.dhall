let prelude = ../prelude/package.dhall

let types = ../types/package.dhall

let mkBash = ./mkBash.dhall

let mkBashWithBinaries =
		λ(Bar : Type)
	  → λ(carrier : types.Carrier Bar)
	  → λ(binaries : List Text)
	  → λ(interval : Natural)
	  → λ(input : Text)
	  → carrier.join
		[ carrier.check
		  ( prelude.List.map
			Text
			types.Check
			(   λ(binary : Text)
			  → { message =
					""
				, assertion =
					types.Assertion.BinaryInPath binary
				}
			)
			binaries
		  )
		, mkBash Bar carrier interval input
		]

in  mkBashWithBinaries
