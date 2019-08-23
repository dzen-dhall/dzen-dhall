{- A variant of mkTransition that allows to specify a list of `from` States. -}
let Event = ../types/Event.dhall

let State = ../types/State.dhall

let Hook = ../types/Hook.dhall

let Transition = ../types/Transition.dhall

let mkTransitions
	: Event → List State → State → Transition
	=   λ(event : Event)
	  → λ(from : List State)
	  → λ(to : State)
	  → { hooks = [] : List Hook, events = [ event ], from = from, to = to }

in  mkTransitions
