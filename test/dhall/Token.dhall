let Token = ./types/Token.dhall

let OpeningTag = ./types/OpeningTag.dhall

let Source = ./types/Source.dhall

let Marquee = ./types/Marquee.dhall

in  [ Token.Open
	  ( OpeningTag.Marquee
		({ framesPerCharacter = 2, width = 3, shouldWrap = False } : Marquee)
	  )
	, Token.Markup "raw"
	, Token.Source
	  (   { updateInterval =
			  Some 1
		  , command =
			  [ "bash" ]
		  , input =
			  "echo 1"
		  , escape = True

		  }
		: Source
	  )
	, Token.Txt "txt"
	, Token.Close
	] : List Token
