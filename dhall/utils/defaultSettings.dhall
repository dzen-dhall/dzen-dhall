let Settings = ../types/Settings.dhall

in    { monitor =
		  0
	  , extraArgs =
		  [ "-ta", "l" ] : List Text
	  , updateInterval =
		  250
	  , font =
		  Some "-*-monospace-medium-r-*-*-14-*-*-*-*-*-*-*"
	  , fontWidth =
		  10
	  }
	: Settings
