let Event = ../types/Event.dhall

let mkEvent : Text → Event = λ(address : Text) → Event.Event address

in  mkEvent
