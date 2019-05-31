let Token = ./src/Token.dhall

let OpeningTag = ./src/OpeningTag.dhall

in  [ Token.Open (OpeningTag.Marquee +1)
	, Token.Raw "raw"
	, Token.Shell "shell"
	, Token.Txt "txt"
	, Token.Close
	] : List Token
