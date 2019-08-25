let Fade = ../types/Fade.dhall

let Slider = ../types/Slider.dhall

let mkSlider
	: Fade → Fade → Natural → Slider
	=   λ(fadeIn : Fade)
	  → λ(fadeOut : Fade)
	  → λ(delay : Natural)
	  → { fadeIn = fadeIn, fadeOut = fadeOut, delay = delay }

in  mkSlider
