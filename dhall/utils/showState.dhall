let State = ../types/State.dhall

let showState
	: State → Text
	= λ(state : State) → merge { State = λ(x : Text) → x } state

in  showState
