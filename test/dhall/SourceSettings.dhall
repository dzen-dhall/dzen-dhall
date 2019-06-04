let SourceSettings : Type = ./src/SourceSettings.dhall

in  { updateInterval =
		Some 1
	, command =
		[ "bash" ]
	, stdin =
		Some "echo hi"
	, escapeMode =
		{ joinLines = True, escapeMarkup = True }
	}
