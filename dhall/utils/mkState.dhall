let State = ../types/State.dhall

let mkState : Text → State = λ(name : Text) → State.State name

in  mkState
