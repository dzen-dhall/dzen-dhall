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
		, escapeMode =
			{ joinLines = False, escapeMarkup = True }
		}

in  mkBash
