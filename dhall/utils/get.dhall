let Variable = ../types/Variable.dhall

let showVariable = ./showVariable.dhall

let get = λ(name : Variable) → "`\$GET ${showVariable name}`"

in  get
