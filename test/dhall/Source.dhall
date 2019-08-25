let Source : Type = ./types/Source.dhall

in  { updateInterval =
		Some 1
	, command =
		[ "bash" ]
	, input =
		"echo hi"
	, escape = True
	}
