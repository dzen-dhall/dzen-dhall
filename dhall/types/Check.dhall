let Assertion = ./Assertion.dhall

let Check : Type = { message : Text, assertion : Assertion }

in  Check
