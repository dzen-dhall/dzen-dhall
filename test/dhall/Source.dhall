let Source : Type = ./src/Source.dhall

in  { updateInterval =
		Some 1
	, command =
		[ "bash" ]
	, input =
		"echo hi"
	, escapeMode =
		{ joinLines = True, escapeMarkup = True }
	}
