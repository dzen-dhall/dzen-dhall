let BarSettings = ./src/BarSettings.dhall

in    { monitor =
		  1
	  , extraFlags =
		  [ "-l", "10" ]
	  , updateInterval =
		  250
	  , font =
		  None Text
	  , fontWidth =
		  None Natural
	  }
	: BarSettings
