let StateTransitionTable = ./src/StateTransitionTable.dhall

let MouseButton = ./src/MouseButton.dhall

in    [ { event = MouseButton.Left, from = "initial", to = "1" }
	  , { event = MouseButton.Left, from = "1", to = "2" }
	  , { event = MouseButton.Left, from = "2", to = "initial" }
	  ]
	: StateTransitionTable
