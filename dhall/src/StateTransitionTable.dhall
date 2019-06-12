let MouseButton = ./MouseButton.dhall

let Slot = ./Slot.dhall

let StateTransitionTable
	: Type
	= List { slots : List Slot, events : List MouseButton, from : List Text, to : Text }

in  StateTransitionTable
