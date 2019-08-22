let Variable = ../types/Variable.dhall

let get = λ(name : Variable) → "`\$GET ${name}`"

in  get
