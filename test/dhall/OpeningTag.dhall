let OpeningTag = ./types/OpeningTag.dhall

let Marquee = ./types/Marquee.dhall

let Direction = ./types/Direction.dhall

let Color = ./types/Color.dhall

in    [ OpeningTag.Marquee { framesPerCharacter = 2, width = 3 }
	  , OpeningTag.FG "red"
	  , OpeningTag.Trim { width = 3, direction = Direction.Right }
	  ]
	: List OpeningTag
