let Variable = ../types/Variable.dhall

let getState = λ(name : Variable) → "`\$GET STATE_${name}`"

in  getState
