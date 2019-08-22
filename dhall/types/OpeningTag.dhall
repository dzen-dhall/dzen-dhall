let AbsolutePosition = ./AbsolutePosition.dhall

let Button = ./Button.dhall

let Color = ./Color.dhall

let Direction = ./Direction.dhall

let Hook = ./Hook.dhall

let Marquee = ./Marquee.dhall

let Padding = ./Padding.dhall

let Position = ./Position.dhall

let Slider = ./Slider.dhall

let State = ./State.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

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
		{ stt : StateTransitionTable, address : Text }
	| StateMapKey :
		State
	| Scope
	>
