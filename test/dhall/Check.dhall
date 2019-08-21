let Assertion = ./types/Assertion.dhall

let Check = ./types/Check.dhall

in  [ { message = "", assertion = Assertion.SuccessfulExit "" : Assertion }
	, { message = "", assertion = Assertion.BinaryInPath "" : Assertion }
	] : List Check
