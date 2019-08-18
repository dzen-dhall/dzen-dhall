let lib = ./lib/index.dhall
let types = ./src/types.dhall
let utils = ./src/utils.dhall

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
	  → let join : List Bar → Bar = carrier.join

		let check : List Check → Bar = carrier.check

		in  join
			[ check
			  [ { message =
					"Did you miss something?"
				, assertion =
					Assertion.BinaryInPath "something"
				}
			  ]
			, check
			  [ { message =
					"Not going to work!"
				, assertion =
					Assertion.SuccessfulExit "[[ \$(date +%u) -lt 6 ]]"
				}
			  ]
			]

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
