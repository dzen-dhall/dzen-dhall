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

		let counter =
			  join
			  [ define var "0"
			  , ca
				Button.Left
				''
				shellVar=${get var}
				${set var "\$(( shellVar - 1 ))"}
				''
				(text "-")
			  , bash 1000 "echo \" ${get var} \""
			  , ca
				Button.Left
				''
				shellVar=${get var}
				${set var "\$(( shellVar + 1 ))"}
				''
				(text "+")
			  ]

		in  join [ scope counter, scope counter ]

in  mkConfigs [ { bar = bar, settings = defaultSettings } ] : List Configuration
