let State = ./State.dhall

let StateMap : Type → Type = λ(Bar : Type) → List { state : State, bar : Bar }

in  StateMap
