{- The output string can be string-interpolated into shell scripts.

 -}
let Event = ../types/Event.dhall

let showEvent = ./showEvent.dhall

let emitEvent =
	  λ(event : Event) → "\$EMIT ${showEvent event}"

in  emitEvent
