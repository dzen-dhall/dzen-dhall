let prelude = ./prelude/package.dhall

let types = ./types/package.dhall

let utils = ./utils/package.dhall

let Assertion = types.Assertion

let BarSettings = types.BarSettings

let Bar = types.Bar

let Configuration = types.Configuration

let Carrier = types.Carrier

let Check = types.Check

let mkConfigs = utils.mkConfigs

let defaultBarSettings : BarSettings = utils.defaultBarSettings

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(carrier : Carrier Bar)
	  → let join = carrier.join

		let check : Text → Assertion → Bar = carrier.check

		in  join
			[ check
			  "Did you miss something?"
			  (Assertion.BinaryInPath "something")
			, check
			  "Not going to work!"
			  (Assertion.SuccessfulExit "[[ \$(date +%u) -lt 6 ]]")
			]

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings }
	  ]
	: List Configuration
