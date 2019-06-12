let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

in  < Marquee :
		Marquee
	| Slider :
		Slider
	| Color :
		Text
	| Automaton :
		StateTransitionTable
	| StateMapKey :
		Text
	| Listener :
		Text
	>
