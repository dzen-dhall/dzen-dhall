let Fade = ../types/Fade.dhall

let VerticalDirection = ../types/VerticalDirection.dhall

let mkFade
	: VerticalDirection → Natural → Natural → Fade
	=   λ(direction : VerticalDirection)
	  → λ(frameCount : Natural)
	  → λ(height : Natural)
	  → { direction = direction, frameCount = frameCount, height = height }

in  mkFade
