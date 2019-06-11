let MouseButton = ./MouseButton.dhall

let StateTransitionTable : Type = List { event : MouseButton, from : Text, to : Text }

in  StateTransitionTable
