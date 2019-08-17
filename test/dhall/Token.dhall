let Token = ./src/Token.dhall

let OpeningTag = ./src/OpeningTag.dhall

let Source = ./src/Source.dhall

let Marquee = ./src/Marquee.dhall

in  [ Token.Open
	  ( OpeningTag.Marquee
		({ framesPerCharacter = 2, width = 3 } : Marquee)
	  )
	, Token.Raw "raw"
	, Token.Source
	  (   { updateInterval =
			  Some 1
		  , command =
			  [ "bash" ]
		  , input =
			  "echo 1"
		  , escapeMode =
			  { joinLines = True, escapeMarkup = True }
		  }
		: Source
	  )
	, Token.Txt "txt"
	, Token.Close
	] : List Token
