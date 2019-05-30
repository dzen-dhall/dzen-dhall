let Token = ./src/Token.dhall

let OpeningTag = ./src/OpeningTag.dhall

let ClosingTag = ./src/ClosingTag.dhall

in    [ Token.Open (OpeningTag.Marquee +1)
	  , Token.Raw "raw"
	  , Token.Shell "shell"
	  , Token.Txt "txt"
	  , Token.Close ClosingTag.Marquee
	  ]
	: List Token
