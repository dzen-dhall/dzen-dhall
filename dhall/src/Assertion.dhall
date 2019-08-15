let Check = ./Check.dhall

let Assertion : Type = { message : Text, check : Check }

in  Assertion
