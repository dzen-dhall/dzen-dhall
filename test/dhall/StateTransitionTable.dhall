let StateTransitionTable = ./types/StateTransitionTable.dhall

let Event = ./types/Event.dhall

let Button = ./types/Button.dhall

let Slot = ./types/Slot.dhall

let Hook = ./types/Hook.dhall

let mkLeftClick =
		λ(from : Text)
	  → λ(to : Text)
	  → [ { slots =
			  [ "a", "b" ] : List Slot
		  , events =
			  [ Event.Mouse Button.Left, Event.Mouse Button.Right ]
		  , hooks =
			  [] : List Hook
		  , from =
			  [ from ]
		  , to =
			  to
		  }
		]

in    mkLeftClick "" "1" # mkLeftClick "1" "2" # mkLeftClick "2" ""
	: StateTransitionTable
