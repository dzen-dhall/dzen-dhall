let VerticalDirection = ./VerticalDirection.dhall

let Slider =
	  { fadeIn :
		  { direction : VerticalDirection, frameCount : Natural }
	  , fadeOut :
		  { direction : VerticalDirection, frameCount : Natural }
	  , delay :
		  Natural
	  }

in  Slider
