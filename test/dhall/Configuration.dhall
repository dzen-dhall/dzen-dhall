let Configuration = ./src/Configuration.dhall

let Token = ./src/Token.dhall

in  [ { bar =
		  [ Token.Close ]
	  , settings =
		  { monitor = 1, extraFlags = [ "-l", "10" ] } : ./src/BarSettings.dhall
	  }
	]
