let StateTransitionTable = ./src/StateTransitionTable.dhall

let Event = ./src/Event.dhall

let Button = ./src/Button.dhall

let Slot = ./src/Slot.dhall

let Hook = ./src/Hook.dhall

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
