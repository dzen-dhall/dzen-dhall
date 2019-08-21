let BarSettings = ./types/BarSettings.dhall

in    { monitor =
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
