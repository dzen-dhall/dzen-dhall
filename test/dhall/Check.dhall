let Assertion = ./src/Assertion.dhall

let Check = ./src/Check.dhall

in  [ { message = "", assertion = Assertion.SuccessfulExit "" : Assertion }
	, { message = "", assertion = Assertion.BinaryInPath "" : Assertion }
	] : List Check
