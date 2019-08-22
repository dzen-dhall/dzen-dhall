let Address = ../types/Address.dhall

let mkAddress : Text → Address = λ(address : Text) → Address.Address address

in  mkAddress
