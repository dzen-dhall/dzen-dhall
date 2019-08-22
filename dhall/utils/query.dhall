let Address = ../types/Address.dhall

let showAddress = ./showAddress.dhall

let query = λ(name : Address) → "`\$GET STATE_${showAddress name}`"

in  query
