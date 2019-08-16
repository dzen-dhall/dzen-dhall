let OpeningTag = ./src/OpeningTag.dhall

let Marquee = ./src/Marquee.dhall

let Color = ./src/Color.dhall

in  [ OpeningTag.Marquee { framesPerCharacter = 2, width = 3 }
	, OpeningTag.FG "red"
	] : List OpeningTag
