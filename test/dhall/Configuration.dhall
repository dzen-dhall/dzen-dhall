let Configuration = ./src/Configuration.dhall

let BarSettings = ./src/BarSettings.dhall

let Token = ./src/Token.dhall

in  [ { bar =
		  [ Token.Close ]
	  , settings =
			{ monitor = 1, extraFlags = [ "-l", "10" ], updateInterval = 250 }
		  : BarSettings
	  }
	]