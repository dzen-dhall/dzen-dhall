let Slot = ./Slot.dhall

let Event = ./Event.dhall

let State = ./State.dhall

let Hook = ./Hook.dhall

let Transition
	: Type
	= { slots :
		  List Slot
	  , events :
		  List Event
	  , from :
		  List State
	  , to :
		  State
	  , hooks :
		  List Hook
	  }

in  Transition
