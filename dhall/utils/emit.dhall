let Event = ../types/Event.dhall

let Shell = ../types/Shell.dhall

let showEvent = ./showEvent.dhall

let emitEvent : Event → Shell = λ(event : Event) → "\$EMIT ${showEvent event}"

in  emitEvent
