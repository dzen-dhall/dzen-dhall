let Slider = ./Slider.dhall

let VerticalDirection = ./VerticalDirection.dhall

in    { fadeIn =
		  { direction = VerticalDirection.Down, frameCount = 9, height = 18 }
	  , fadeOut =
		  { direction = VerticalDirection.Down, frameCount = 9, height = 18 }
	  , delay =
		  5000
	  }
	: Slider
