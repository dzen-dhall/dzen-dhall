let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let Color = ./Color.dhall

let Direction = ./Direction.dhall

let Position = ./Position.dhall

let Padding = ./Padding.dhall

let AbsolutePosition = ./AbsolutePosition.dhall

let Button = ./Button.dhall

let Hook = ./Hook.dhall

in  < Marquee :
		Marquee
	| Slider :
		Slider
	| FG :
		Color
	| BG :
		Color
	| P :
		Position
	| PA :
		AbsolutePosition
	| CA :
		{ button : Button, command : Text }
	| IB
	| Padding :
		{ width : Natural, padding : Padding }
	| Trim :
		{ width : Natural, direction : Direction }
	| Automaton :
		{ stt : StateTransitionTable, id : Text }
	| StateMapKey :
		Text
	| Listener :
		Text
	| Scope
	>
