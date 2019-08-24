let prelude = ./prelude/package.dhall

let types = ./types/package.dhall

let utils = ./utils/package.dhall

let Bar = types.Bar

let Settings = types.Settings

let Button = types.Button

let Carrier = types.Carrier

let Configuration = types.Configuration

let Shell = types.Shell

let Source = types.Source

let Transition = types.Transition

let Variable = types.Variable

let mkConfigs = utils.mkConfigs

let defaultSettings : Settings = utils.defaultSettings

let mkVariable : Text → Variable = utils.mkVariable

let get : Variable → Shell = utils.get

let set : Variable → Text → Shell = utils.set

let bar
	: Bar
	=   λ(Bar : Type)
	  → λ(cr : Carrier Bar)
	  → let text : Text → Bar = cr.text

		let join : List Bar → Bar = cr.join

		let scope : Bar → Bar = cr.scope

		let define : Variable → Text → Bar = cr.define

		let ca : Button → Shell → Bar → Bar = cr.ca

		let bash : Natural → Text → Bar = utils.mkBash Bar cr

		let var = mkVariable "MyVariable"

		in	join
		  [ define var "0"
		  -- ^ set a default value (optional)

		  , ca
			Button.Left
			''
			shellVar=${get var}
			${set var "$(( shellVar - 1 ))"}
			''
			(text "-")
		  -- ^ a button that decreases the value

		  , bash 500
		    ''
		    echo " ${get var} "
		    ''
		  -- ^ a bar that prints the value

		  , ca
			Button.Left
			''
			shellVar=${get var}
			${set var "$(( shellVar + 1 ))"}
			''
			(text "+")
		  -- ^ a button that increases the value

		  ]

in  mkConfigs [ { bar = bar, settings = defaultSettings } ] : List Configuration
