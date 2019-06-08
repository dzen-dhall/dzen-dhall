let Source : Type = ./src/Source.dhall

in  { updateInterval =
		Some 1
	, command =
		[ "bash" ]
	, stdin =
		Some "echo hi"
	, escapeMode =
		{ joinLines = True, escapeMarkup = True }
	}
