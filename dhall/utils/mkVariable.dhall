let Variable = ../types/Variable.dhall

let mkVariable : Text → Variable = λ(address : Text) → Variable.Variable address

in  mkVariable
