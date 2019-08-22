let StateTransitionTable = ./types/StateTransitionTable.dhall

let Event = ./types/Event.dhall

let State = ./types/State.dhall

let Button = ./types/Button.dhall

let Hook = ./types/Hook.dhall

let mkState = ./utils/mkState.dhall

let mkLeftClick =
		λ(from : State)
	  → λ(to : State)
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

in      mkLeftClick (mkState "") (mkState "1")
	  # mkLeftClick (mkState "1") (mkState "2")
	  # mkLeftClick (mkState "2") (mkState "")
	: StateTransitionTable
