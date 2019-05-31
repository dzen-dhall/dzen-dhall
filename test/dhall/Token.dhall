let Token = ./src/Token.dhall

let OpeningTag = ./src/OpeningTag.dhall

let SourceSettings = ./src/SourceSettings.dhall

in  [ Token.Open (OpeningTag.Marquee +1)
	, Token.Raw "raw"
	, Token.Source
	  (   { updateInterval =
			  Some 1
		  , command =
			  [ "bash" ]
		  , stdin =
			  Some "echo 1"
		  }
		: SourceSettings
	  )
	, Token.Txt "txt"
	, Token.Close
	] : List Token
