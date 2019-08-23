let Event = ../types/Event.dhall

let State = ../types/State.dhall

let Hook = ../types/Hook.dhall

let Transition = ../types/Transition.dhall

let mkTransition
	: Event → State → State → Transition
	=   λ(event : Event)
	  → λ(from : State)
	  → λ(to : State)
	  → { hooks = [] : List Hook, events = [ event ], from = [ from ], to = to }

in  mkTransition
