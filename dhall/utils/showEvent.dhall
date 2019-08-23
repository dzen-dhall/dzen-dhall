let Event = ../types/Event.dhall

let showEvent
	: Event → Text
	= λ(event : Event) → merge { Event = λ(name : Text) → name } event

in  showEvent
