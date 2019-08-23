{- An example showing how to define automata. -}
let prelude = ./prelude/package.dhall

let types = ./types/package.dhall

let utils = ./utils/package.dhall

let Address = types.Address

let Bar = types.Bar

let BarSettings = types.BarSettings

let Button = types.Button

let Carrier = types.Carrier

let Configuration = types.Configuration

let Event = types.Event

let Hook = types.Hook

let State = types.State

let StateMap = types.StateMap

let Transition = types.Transition

let mkConfigs = utils.mkConfigs

let defaultBarSettings : BarSettings = utils.defaultBarSettings

let emit : Event → Text = utils.emit

let ON : State = utils.mkState "ON"

let OFF : State = utils.mkState "OFF"

let Toggle : Event = Event.Custom "Toggle"

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(carrier : Carrier Bar)
	  → let text : Text → Bar = carrier.text

		let ca : Button → Text → Bar → Bar = carrier.ca

		let automaton
			: Address → List Transition → StateMap Bar → Bar
			= carrier.automaton

		let stateTransitionTable
			: List Transition
			= [ { hooks =
					[] : List Hook
				, events =
					[ Event.Mouse Button.Left ]
				, from =
					[ ON ]
				, to =
					OFF
				}
			  , { hooks =
					[] : List Hook
				, events =
					[ Event.Mouse Button.Left ]
				, from =
					[ OFF ]
				, to =
					ON
				}
			  ]

		let stateMap
			: StateMap Bar
			= [ { state = OFF, bar = text "Switcher is OFF" }
			  , { state = ON, bar = text "Switcher is ON" }
			  ]

		let address : Address = utils.mkAddress "MY_AUTOMATON"

		in  ca Button.Left (emit Toggle) (automaton address stateTransitionTable stateMap)

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
