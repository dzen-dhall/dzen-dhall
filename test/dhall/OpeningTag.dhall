let OpeningTag = ./src/OpeningTag.dhall

let MarqueeSettings = ./src/MarqueeSettings.dhall

in  [ OpeningTag.Marquee { speed = +1, framesPerCharacter = 2, width = 3 }
	, OpeningTag.Color "red"
	]
