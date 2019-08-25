{- You can use this file as your `config.dhall`. -}
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

let defaults = utils.defaults

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

		let bash : Natural → Text → Bar = utils.mkBash Bar cr

		let counter = bash 1000 "notify-send `date +%S`"

		-- You'll see one notification per second, not two:
		in  join [ counter, counter ]


in  mkConfigs [ { bar = bar, settings = defaults.settings } ] : List Configuration
