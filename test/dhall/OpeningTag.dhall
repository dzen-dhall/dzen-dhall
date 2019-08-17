let OpeningTag = ./src/OpeningTag.dhall

let Marquee = ./src/Marquee.dhall

let Direction = ./src/Direction.dhall

let Color = ./src/Color.dhall

in    [ OpeningTag.Marquee { framesPerCharacter = 2, width = 3 }
	  , OpeningTag.FG "red"
	  , OpeningTag.Trim { width = 3, direction = Direction.Right }
	  ]
	: List OpeningTag
