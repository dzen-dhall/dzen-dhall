let VerticalDirection = ./VerticalDirection.dhall

let Slider =
	  { fadeIn :
		  { direction :
			  VerticalDirection
		  , frameCount :
			  Natural
		  , height :
			  Natural
		  }
	  , fadeOut :
		  { direction :
			  VerticalDirection
		  , frameCount :
			  Natural
		  , height :
			  Natural
		  }
	  , delay :
		  Natural
	  }

in  Slider
