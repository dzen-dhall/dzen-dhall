let OpeningTag = ./src/OpeningTag.dhall

let Marquee = ./src/Marquee.dhall

in  [ OpeningTag.Marquee { framesPerCharacter = 2, width = 3 }
	, OpeningTag.Color "red"
	] : List OpeningTag
