let types = ../types/package.dhall

let Event = types.Event

let Button = types.Button

let showButton : Button → Text = ./showButton.dhall

let showEvent
	: Event → Text
	=   λ(event : Event)
	  → merge { Mouse = showButton, Custom = λ(name : Text) → name } event

in  showEvent
