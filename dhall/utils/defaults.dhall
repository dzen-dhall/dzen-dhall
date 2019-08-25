{- Well-chosen default values for various types. -}
let types = ../types/package.dhall

let Settings = types.Settings

let Button = types.Button

let VerticalDirection = types.VerticalDirection

let Fade = types.Fade

let Slider = types.Slider

let Marquee = types.Marquee

let Source = types.Source

let fade
	: Fade
	= { direction = VerticalDirection.Up, frameCount = 5, height = 10 }

let slider : Slider = { fadeIn = fade, fadeOut = fade, delay = 5000 }

let settings
	: Settings
	= { monitor =
		  0
	  , extraArgs =
		  [ "-ta", "l" ] : List Text
	  , updateInterval =
		  250
	  , font =
		  Some "-*-monospace-medium-r-*-*-14-*-*-*-*-*-*-*"
	  , fontWidth =
		  8
	  }

let source
	: Source
	= { command =
		  [ "bash" ]
	  , input =
		  ""
	  , updateInterval =
		  Some 1000
	  , escape =
		  True
	  }

let button : Button = Button.Left

let marquee : Marquee = { framesPerCharacter = 8, width = 32, shouldWrap = False }

let defaults =
	  { settings =
		  settings
	  , fade =
		  fade
	  , slider =
		  slider
	  , source =
		  source
	  , button =
		  button
	  , marquee =
		  marquee
	  }

in  defaults
