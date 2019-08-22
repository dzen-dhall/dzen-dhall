let StateTransitionTable = ./types/StateTransitionTable.dhall

let Event = ./types/Event.dhall

let Button = ./types/Button.dhall

let Hook = ./types/Hook.dhall

let mkLeftClick =
		λ(from : Text)
	  → λ(to : Text)
	  → [ { events =
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
