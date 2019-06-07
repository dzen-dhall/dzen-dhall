let Token = ./src/Token.dhall

let OpeningTag = ./src/OpeningTag.dhall

let SourceSettings = ./src/SourceSettings.dhall

let MarqueeSettings = ./src/MarqueeSettings.dhall

in  [ Token.Open
	  ( OpeningTag.Marquee
		({ framesPerCharacter = 2, width = 3 } : MarqueeSettings)
	  )
	, Token.Raw "raw"
	, Token.Source
	  (   { updateInterval =
			  Some 1
		  , command =
			  [ "bash" ]
		  , stdin =
			  Some "echo 1"
		  , escapeMode =
			  { joinLines = True, escapeMarkup = True }
		  }
		: SourceSettings
	  )
	, Token.Txt "txt"
	, Token.Close
	] : List Token
