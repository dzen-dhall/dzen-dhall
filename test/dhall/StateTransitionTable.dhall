let StateTransitionTable = ./src/StateTransitionTable.dhall

let MouseButton = ./src/MouseButton.dhall

let Slot = ./src/Slot.dhall

let mkLeftClick =
		λ(from : Text)
	  → λ(to : Text)
	  → { slots =
			[ "a", "b" ] : List Slot
		, events =
			[ MouseButton.Left, MouseButton.Right ]
		, from =
			[ from ]
		, to =
			to
		}

in    [ mkLeftClick "" "1", mkLeftClick "1" "2", mkLeftClick "2" "" ]
	: StateTransitionTable
