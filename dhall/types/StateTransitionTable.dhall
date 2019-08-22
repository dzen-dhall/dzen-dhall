let Transition = ./Transition.dhall

let StateTransitionTable
	: Type
	= List Transition

in  StateTransitionTable
