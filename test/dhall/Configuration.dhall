let Configuration = ./types/Configuration.dhall

let BarSettings = ./types/BarSettings.dhall

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
				None Natural
			}
		  : BarSettings
	  }
	]
