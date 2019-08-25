{- An example showing how to define automata.
   You can use this file as your `config.dhall`.
-}
let prelude = ./prelude/package.dhall

let types = ./types/package.dhall

let utils = ./utils/package.dhall

let Address = types.Address

let Bar = types.Bar

let Settings = types.Settings

let Button = types.Button

let Carrier = types.Carrier

let Configuration = types.Configuration

let Event = types.Event

let Hook = types.Hook

let State = types.State

let StateMap = types.StateMap

let Transition = types.Transition

let mkConfigs = utils.mkConfigs

let mkState = utils.mkState

let mkEvent = utils.mkEvent

let mkAddress = utils.mkAddress

let mkTransition = utils.mkTransition

let defaults = utils.defaults

let emit : Event → Text = utils.emit

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(carrier : Carrier Bar)
	  → let text : Text → Bar = carrier.text

		let ca : Button → Text → Bar → Bar = carrier.ca

		let automaton
			: Address → List Transition → StateMap Bar → Bar
			= carrier.automaton

		let OFF : State = mkState ""

		let ON : State = mkState "ON"

		let Toggle : Event = mkEvent "Toggle"

		let address : Address = mkAddress "MY_AUTOMATON"

		let stateTransitionTable
			: List Transition
			= [ mkTransition Toggle ON OFF, mkTransition Toggle OFF ON ]

		let stateMap
			: StateMap Bar
			= [ { state = OFF, bar = text "Switcher is OFF" }
			  , { state = ON, bar = text "Switcher is ON" }
			  ]

		in  ca
			Button.Left
			(emit Toggle)
			(automaton address stateTransitionTable stateMap)

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaults.settings : Settings } ]
	: List Configuration
