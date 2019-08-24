let Settings = ./types/Settings.dhall

in    { monitor =
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
