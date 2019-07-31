let Button = ./Button.dhall

let Slot = ./Slot.dhall

let Hook = ./Hook.dhall

let State = ./State.dhall

let StateTransitionTable
	: Type
	= List
	  { slots :
		  List Slot
	  , events :
		  List Button
	  , from :
		  List State
	  , to :
		  State
	  , hooks :
		  List Hook
	  }

in  StateTransitionTable
