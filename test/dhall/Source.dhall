let Source : Type = ./src/Source.dhall

in  { updateInterval =
		Some 1
	, command =
		[ "bash" ]
	, input =
		Some "echo hi"
	, escapeMode =
		{ joinLines = True, escapeMarkup = True }
	}
