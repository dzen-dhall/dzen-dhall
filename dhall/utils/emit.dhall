{- Emit an event to a given slot.

The output string can be string-interpolated into shell scripts.

 -}
let Event = ../types/Event.dhall

let Slot = ../types/Slot.dhall

let showEvent = ./showEvent.dhall

let emitEvent =
	  λ(slot : Slot) → λ(event : Event) → "\$EMIT ${slot} ${showEvent event}"

in  emitEvent
