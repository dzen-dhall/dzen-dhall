let Configuration = ./types/Configuration.dhall

let Settings = ./types/Settings.dhall

let Token = ./types/Token.dhall

in  [ { bar =
		  [ Token.Close ]
	  , settings =
			{ monitor =
				1
			, extraArgs =
				[ "-l", "10" ]
			, updateInterval =
				250
			, font =
				None Text
			, fontWidth =
				10
			}
		  : Settings
	  }
	]
