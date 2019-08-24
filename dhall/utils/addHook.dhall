let types = ../types/package.dhall

let Transition = types.Transition

let Hook = types.Hook

let addHook
	: Hook → Transition → Transition
	=   λ(hook : Hook)
	  → λ(transition : Transition)
	  → transition ⫽ { hooks = [ hook ] # transition.hooks }

in  addHook
