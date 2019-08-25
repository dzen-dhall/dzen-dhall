let Marquee = ../types/Marquee.dhall

let mkMarquee
	: Natural → Natural → Bool → Marquee
	=   λ(framesPerCharacter : Natural)
	  → λ(width : Natural)
	  → λ(shouldWrap : Bool)
	  → { framesPerCharacter =
			framesPerCharacter
		, width =
			width
		, shouldWrap =
			shouldWrap
		}

in  mkMarquee
